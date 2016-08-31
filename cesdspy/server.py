import requests
import logging

log = logging.getLogger(__name__)

def get(api_url):
    res = requests.get(api_url)
    try:
        res.raise_for_status()
        return res.json()
    except:
        log.error('%s - %s', res.status_code, res.text)

def issue_command(api_url, body={}):
    url = '{}/{}'.format(api_url, 'command')
    res = request.post(api_url, data=body)
    try:
        res.raise_for_status()
        return res.json()
    except:
        log.error('%s - %s', res.status_code, res.text)
        raise
