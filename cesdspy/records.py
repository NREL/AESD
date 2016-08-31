import requests
import logging

log = logging.getLogger(__name__)

root = 'models'
endpoint = 'records'

def get(api_url, model_id, record_id, query = {}):
    url = '{}/{}/{}/{}/{}'.format(api_url, root, model_id, endpoint, record_id)
    res = requests.get(url, params = query)
    try:
        res.raise_for_status()
        return res.json()
    except:
        log.error('%s - %s', res.status_code, res.text)
        raise

def list(api_url, model_id, query = {}):
    url = '{}/{}/{}/{}'.format(api_url, root, model_id, endpoint)
    res = requests.get(url, params = query)
    try:
        res.raise_for_status()
        return res.json()
    except:
        log.error('%s - %s', res.status_code, res.text)
        raise
