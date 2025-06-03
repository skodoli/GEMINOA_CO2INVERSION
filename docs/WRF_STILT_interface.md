Documentation for the WRF-STILT interface
A technical description of the WRF-STILT interface can be found in:

  Nehrkorn, T., J. Eluszkiewicz, S. C. Wofsy, J. C. Lin, C. Gerbig,
  M. Longo, and S. Freitas, 2010: Coupled weather research and
  forecasting - stochastic time-inverted lagrangian transport
  ({WRF-STILT}) model. Meteor. Atmos. Phys., 107 (1), 51-64,
  doi:10.1007/s00703-010-0068-x.
1. Instructions for activating the WRFV3 "avgflx" package:

Starting with version 3.2 of WRF, two steps are needed to activate the output of time-averaged winds (and convective mass fluxes, if using the Grell-Devenyi cumulus scheme) needed for the WRF-STILT interface:

1.1. Add two lines to the namelist file, to the "&dynamics" namelist:

 do_avgflx_em                        = 1, 1, 1, 1, 1, 1, 1, 1, 1,
 do_avgflx_cugd                      = 1, 1, 1, 1, 1, 1, 1, 1, 1,
(This is a simple on/off switch, by domain, 1=on/0=off)

1.2. Activate "history output" for some additional variables in the Registry/Registry.EM file (Registry.EM_COMMON in v3.4).

This will require recompilation. The variables are:

muu, muv,mut, alt.

Also recommended: tke (this is not strictly necessary for the WRF-STILT interface, but potentially useful when using PBL schemes that compute TKE).
