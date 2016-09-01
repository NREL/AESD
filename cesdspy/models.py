import requests
import logging

log = logging.getLogger(__name__)

root = 'models'

def get(api_url, model_id):
    """Get the model description object

    Args:
        api_url (string): The base url to query
        model_id (string): The ID of the model to get

    Returns:
        json: The model information

    Example:

        .. code-block:: python

            import cesdspy
            cesdspy.models.get('http://1lv11lamb01.nrel.gov:8091/models/RSF2v0')

        .. code-block:: javascript
            :caption: **cesdspy.models.get response**

            {
              "record_id_var": "time",
              "variables": [
                {
                  "display": {
                    "shortlabel": "Time",
                    "label": "Time Stamp"
                  },
                  "domain": {
                    "set": {
                      "options": []
                    }
                  },
                  "is_input": false,
                  "var_id": "time"
                },
                {
                  "display": {
                    "shortlabel": "Seconds",
                    "label": "POSIX Seconds"
                  },
                  "domain": {
                    "interval": {
                      "bounds": [
                        315558000,
                        null
                      ]
                    }
                  },
                  "is_input": false,
                  "var_id": "epoch",
                  "units": {
                    "scale": 1,
                    "SI": [
                      0,
                      0,
                      1,
                      0,
                      0,
                      0,
                      0,
                      0
                    ]
                  }
                },
                ...
              ],
              "model_id": "RSF2v0",
              "time_key": "epoch",
              "model_uri": "http://www.nrel.gov/sustainable_nrel/rsf.html#RSF2v0",
              "generation": 1472745315,
              "description": "Selected power meters from the RSF 2",
              "label": "RSF 2 Version 0",
              "record_count": 189101,
              "tags": {
                "DC.source": "https://skyspark-ops.nrel.gov/proj/nrel",
                "DC.description": "Selected power meters from the RSF 2",
                "DC.creator": "Brian W Bush <brian.bush@nrel.gov>"
              }
            }
    """
    url = '{}/{}/{}'.format(api_url, root, model_id)
    res = requests.get(url)
    try:
        res.raise_for_status()
        return res.json()
    except:
        log.error('%s - %s', res.status_code, res.text)

def list(api_url):
    """get a set model id

    Args:
        api_url (string): The base url to query

    Returns:
        array: The set of model IDs

    Example:

        .. code-block:: python

            import cesdspy
            cesdspy.models.list('http://1lv11lamb01.nrel.gov:8091/')

        .. code-block:: python
            :caption: **cesdspy.models.list response**

            ["RSF2v0"]
    """
    res = requests.get(api_url)
    try:
        res.raise_for_status()
        return res.json()['models']
    except:
        log.error('%s - %s', res.status_code, res.text)

def issue_command(api_url, model_id, body={}):
    """Issue a command to a model

    Args:
        api_url (str): The base url to query
        model_id (string): The ID of the model to get
        body (json): The command to issue

    Returns:
        json: The result of the issued command

    Example:

        .. code-block:: python

            import cesdspy
            body = {
                "command": "get_model_strategy"
            }
            cesdspy.models.issue_command('http://1lv11lamb01.nrel.gov:8091/', 'RSF2v0', body)

        .. code-block:: javascript
            :caption: **cesdspy.models.issue_command response**

            {
                "result" : "TODO"
            }
    """
    url = '{}/{}/{}/{}'.format(api_url, root, model_id, 'command')
    res = requests.get(url, data=body)
    try:
        res.raise_for_status()
        return res.json()
    except:
        log.error('%s - %s', res.status_code, res.text)
    print(url)
