#!/bin/csh -f

### Specify the base directory
set baseDir  = "/mnt/data2/skodoli/INVERSION/BEACON_inv_tr"
set baseName = "est_fluxes"
set tName    = "${baseDir}/templates/template_${baseName}.jl"
set bName    = "${baseDir}/templates/template_batch_day.brc"
set bNameALT = "${baseDir}/templates/template_batch_day2.brc"
set sName    = "${baseDir}/batch/temporary_submit.brc"

### Specify the year to run
set yyyy = "2023"
#set yyyy = "2019"
#set yyyy = "2020"

### Define the inversion window and number of back days
set invWindow = "1"  # Days to use
set backHours = "36" # Number of back days to use

### Are we doing cross validation?
set cross_validate = 0  # zero for false, one for true
set kFold          = 3 

### Add the months
#@ iMONT = 1
#while ( $iMONT <= 12 )
   # Figure out how many days in the month
 #  @ dayMax = 31
  # if ( $iMONT == 4 || $iMONT == 6 || $iMONT == 9 || $iMONT == 11 ) then
   #   @ dayMax = 30
   #else if ( $iMONT == 2 ) then
    #  @ dayMax = 28
    #  if ( "${yyyy}" == "2020" ) then
     #    @ dayMax = 29
     # endif
  # endif

   # Loop through
  # @ dayLoop = 1
  # while ( $dayLoop <= $dayMax )
  #    set daySAVE = `printf "%02d" $dayLoop`
   #   set monSAVE = `printf "%02d" $iMONT`
    #  if ( $iMONT == 1 && $dayLoop == 1 ) then
     #    set day   = ($daySAVE)
      #   set month = ($monSAVE)
      #else
       #  set day   = ($day:q   $daySAVE)
       #  set month = ($month:q $monSAVE)
     # endif
     # @ dayLoop = $dayLoop + 1
  # end
  # @ iMONT = $iMONT + 1
#end
   
### Manual example
## August
#set month = ("06" "06" "06" "06" "06" "06" "06" "06" "06" "06" "06" "06" "06" "06" "06" "06" "06" "06" "06" "06" "06" "06" "06" "06" "06" "06" "06" "06" "06" "06" )
#set day   = ("01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" "21" "22" "23" "24" "25" "26" "27" "28" "29" "30" )
## April2022
#set month = ("08" )
#set day   = ( "08" )
## February
#set month = ($month:q "02" "02" "02" "02" "02" "02" "02" "02" "02" "02" "02" "02" "02" "02" "02" "02" "02" "02" "02" "02" "02" "02" "02" "02" "02" "02" "02" "02")
#set day   = ($day:q   "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" "21" "22" "23" "24" "25" "26" "27" "28")
set month = ("09" "09" "09" "09" "09" "09" "09" "09" "09" "09" "09" "09" "09" "09" "09" "09" "09" "09" "09" "09" "09" "09" "09" "09" "09" "09" "09" "09" "09" "09")
set day   = ("01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" "21" "22" "23" "24" "25" "26" "27" "28" "29" "30")
### Create the files
rm -f ${baseDir}/${baseName}_????????.jl > /dev/null    # Clear old files
rm -f ${baseDir}/${baseName}_????????_*.jl > /dev/null  # Clear old files
rm -f ${sName} > /dev/null                              # Clear old files
rm -f ${baseDir}/batch/submit_*.brc > /dev/null         # Clear old files
rm -f ${baseDir}/batch/may.csh > /dev/null       # Clear old files
@ i = 0
foreach iter ($day)

   ### Get month and day
   @ i++
   set mm = ${month[$i]}
   set dd = ${day[$i]}

   ### Initialize our submission script
   if ("${yyyy}${mm}${dd}" == "20200213" || \
       "${yyyy}${mm}${dd}" == "20200220" || \
       "${yyyy}${mm}${dd}" == "20200308" || \
       "${yyyy}${mm}${dd}" == "20200309" || \
       "${yyyy}${mm}${dd}" == "20200310" || \
       "${yyyy}${mm}${dd}" == "20200311" || \
       "${yyyy}${mm}${dd}" == "20200319" || \
       "${yyyy}${mm}${dd}" == "20200320" || \
       "${yyyy}${mm}${dd}" == "20200321" || \
       "${yyyy}${mm}${dd}" == "20200408" || \
       "${yyyy}${mm}${dd}" == "20200409") then
      cp ${bNameALT} ${sName}
   else
      cp ${bName} ${sName}
   endif
   echo 'julia -p $nProcs BaseName_YearYearMonthMonthDayDay_DAILY.jl > $runDir/batch/log/log_YearYearMonthMonthDayDay_DAILY.txt' >> ${sName} 
   
   ### Update the template
   # Filenames
   set fName = "${baseDir}/${baseName}_${yyyy}${mm}${dd}_DAILY.jl"
   # Copy the template
   sed -e "s:YearYear:${yyyy}:g" \
       -e "s:MonthMonth:${mm}:g" \
       -e "s:DayDay:${dd}:g" \
       -e "s:WindowWindow:${invWindow}:g" \
       -e "s:BackHoursBackHours:${backHours}:g" \
       -e "s:CrossValidCrossValid:false:g" \
       -e "s:kFoldkFold:10  :g" \
       -e "s:kIndkInd:1   :g" \
       -e "s:SuffixSuffix:_DAILY:g" \
       ${tName} > ${fName}
   chmod 744 ${fName}

   ### Build the cross-validation?
   if ($cross_validate == 1) then
      @ kInd = 1
      while ($kInd <= $kFold)
         # Make the different iterations
         set fName = "${baseDir}/${baseName}_${yyyy}${mm}${dd}_DAILY_${kInd}.jl"
         sed -e "s:YearYear:${yyyy}:g" \
             -e "s:MonthMonth:${mm}:g" \
             -e "s:DayDay:${dd}:g" \
             -e "s:WindowWindow:${invWindow}:g" \
             -e "s:BackHoursBackHours:${backHours}:g" \
             -e "s:CrossValidCrossValid:true :g" \
             -e "s:kFoldkFold:${kFold}  :g" \
             -e "s:kIndkInd:${kInd}   :g" \
             -e "s:SuffixSuffix:_DAILY:g" \
             ${tName} > ${fName}
         chmod 744 ${fName}
         # Add this iteration to the submission script and modify the number
         echo 'julia -p $nProcs BaseName_YearYearMonthMonthDayDay_DAILY_kIndkInd.jl > $runDir/batch/log/log_YearYearMonthMonthDayDay_DAILY_kIndkInd.txt' >> ${sName} 
         sed -i -e "s:kIndkInd:${kInd}:g" ${sName}
         # Iterate
         @ kInd++
      end
   endif

   ### Finalize the submission script
   # Change year, month, day
   sed -i -e "s:BaseName:${baseName}:g" \
          -e "s:YearYear:${yyyy}:g" \
          -e "s:MonthMonth:${mm}:g" \
          -e "s:DayDay:${dd}:g" \
          ${sName}
   # Append to the end of our submission script
   echo ' '                                                           >> ${sName}
   echo '### END of job'                                              >> ${sName}
   echo 'echo "Job complete: "`date`'                                 >> ${sName}
   echo 'exit(0)'                                                     >> ${sName}
   echo ' '                                                           >> ${sName}
   echo '### =======================================================' >> ${sName}
   echo '### =                        E N D                        =' >> ${sName}
   echo '### =======================================================' >> ${sName}
   # Change the name
   mv ${sName} ${baseDir}/batch/submit_${yyyy}${mm}${dd}_DAILY.brc
   chmod 744 ${baseDir}/batch/submit_${yyyy}${mm}${dd}_DAILY.brc
   # Append this to our run script
   echo "sbatch submit_${yyyy}${mm}${dd}_DAILY.brc" >> ${baseDir}/batch/may.csh
   chmod 744 ${baseDir}/batch/may.csh

end

exit(0)
