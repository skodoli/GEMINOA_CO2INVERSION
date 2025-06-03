# Requires Python 2.7+
import logging
from pprint import pprint
import requests
import time

HEX_API_AUTHORIZATION_TOKEN = open("nuwave_key").readline().strip()


def hex_api_request(method, url, params=None, data=None, retries=4, retry_interval=5, timeout=60):
    """
    Make a request to the NuWave HEX API.

    Supports:
    * Full request and response logging
    * Transparent handling of large (paginated) and smaller (non-paginated) results
    * Server-side rate limits (HTTP 429 with Retry-After header)
    * Exponential backoff retries (All other HTTP errors)

    :param method: The HTTP method to use. E.g. 'GET'
    :param url: The API URL to use. E.g. 'v1/device-status/ABCD1234'
    :param params: Query parameters. E.g. {'cursor':'ZYX','time_start':'2020-10-01T00:00:00Z','time_end':'2020-10-02T00:00:00Z'}
    :param data: Request body for POST, PUT, PATCH. Should be JSON serializable. E.g. {'name':'Main Office','timezone':'Europe/Dublin'}
    """
    r_url = 'https://hex2.nuwavesensors.com/api/' + url
    r_params = {} if params == None else params.copy()
    r_headers = {
        'Content-Type': 'application/json',
        'Authorization': 'token ' + HEX_API_AUTHORIZATION_TOKEN,
    }
    results = []
    attempt = 0
    finished = False
    while not finished:
        logging.info('hex_api_request: {} {} {}'.format(method, r_url, r_params))
        try:
            response = requests.request(method, r_url, headers=r_headers, params=r_params, json=data, timeout=timeout)
            log_headers = dict((key, response.headers.get(key)) for key in ['HTTP_X_REQUEST_ID', 'Content-Length'])
            logging.info('hex_api_request: {}, Headers: {}'.format(response, log_headers))
            response.raise_for_status()     # Raise exception for HTTP errors (status code >= 400)
        except requests.RequestException as e:
            err_msg = 'hex_api_request: Request failed: {}, response.content: {}'.format(e, e.response.content[:1024] if e.response != None else None)
            if attempt < retries:
                if e.response != None and 'Retry-After' in e.response.headers:
                    delay = int(e.response.headers['Retry-After'])
                else:
                    delay = retry_interval * 2**(attempt)
                attempt += 1
                logging.warning(err_msg + '. Retrying after {} seconds (attempt {})'.format(delay, attempt))
                time.sleep(delay)
                continue
            else:
                logging.error(err_msg)
                raise
        content = response.json()
        if 'next' in content and 'previous' in content and 'results' in content:
            # Paginated response
            results.extend(content['results'])
            if content['next'] != None:
                logging.info('hex_api_request: Fetching next page of results')
                r_params['cursor'] = content['next']
                attempt = 0
            else:
                finished = True
        else:
            # Unpaginated response
            results = content
            finished = True
    return results


# Show API requests and responses logs as they happen
logging.basicConfig(level=logging.INFO, format='%(asctime)s %(levelname)s %(message)s', datefmt='%Y-%m-%dT%H:%M:%S%z')

# Get metric history of all devices in the last day
result = hex_api_request('GET', 'v1/metric-history/', params={'time_start': '1 day', 'time_end': 'now'})
pprint(result)

# Get metric history of 2 devices in the given time window
result2 = hex_api_request('GET', 'v1/metric-history/', params={'device_ids': '000D6F000FF7DD90,000D6F000FF7DD91', 'time_start': '2020-10-01T00:00:00Z', 'time_end': '2020-10-02T00:00:00Z'})
pprint(result2)

# Get the status of all devices
result3 = hex_api_request('GET', 'v1/device-status/')
pprint(result3)

# Change the name and timezone of one device
result4 = hex_api_request('PATCH', 'v1/device-preferences/000D6F000FF7DD90/', data={'name': 'Main Office', 'timezone': 'Europe/Dublin'})
pprint(result4)

# Get the status of one device
result5 = hex_api_request('GET', 'v1/device-status/000D6F000FF7DD90/')
pprint(result5)
