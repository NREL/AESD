import requests
import logging

log = logging.getLogger(__name__)

root = 'models'

def get(api_url, model_id):
    url = '{}/{}/{}'.format(api_url, root, model_id)
    res = requests.get(url)
    try:
        res.raise_for_status()
        return res.json()
    except:
        log.error('%s - %s', res.status_code, res.text)
        raise

def list(api_url):
    res = requests.get(api_url)
    try:
        res.raise_for_status()
        return res.json()['models']
    except:
        log.error('%s - %s', res.status_code, res.text)
        raise

def issue_command(api_url, model_id, command):
    url = '{}/{}/{}/{}'.format(api_url, root, model_id, 'command')
    print(url)
