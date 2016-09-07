import requests
import logging
from utils.errors import buildErrorPacket

log = logging.getLogger(__name__)

root = 'models'

def get(api_url, model_id):
    """Get model information.

    Args:
        api_url (string): A url to query
        model_id (string): A model ID

    Returns:
        json: The model information

    Example:

        .. code-block:: python

            import cesdspy
            cesdspy.models.get('http://1lv11lamb01.nrel.gov:8091', 'RSF2v0')

            # Returns:
            # {
            #   "record_id_var": "time",
            #   "variables": [
            #     {
            #       "display": {
            #         "shortlabel": "Time",
            #         "label": "Time Stamp"
            #       },
            #       "domain": {
            #         "set": {
            #           "options": []
            #         }
            #       },
            #       "is_input": false,
            #       "var_id": "time"
            #     },
            #     {
            #       "display": {
            #         "shortlabel": "Seconds",
            #         "label": "POSIX Seconds"
            #       },
            #       "domain": {
            #         "interval": {
            #           "bounds": [
            #             315558000,
            #             null
            #           ]
            #         }
            #       },
            #       "is_input": false,
            #       "var_id": "epoch",
            #       "units": {
            #         "scale": 1,
            #         "SI": [
            #           0,
            #           0,
            #           1,
            #           0,
            #           0,
            #           0,
            #           0,
            #           0
            #         ]
            #       }
            #     },
            #     ...
            #   ],
            #   "model_id": "RSF2v0",
            #   "time_key": "epoch",
            #   "model_uri": "http://www.nrel.gov/sustainable_nrel/rsf.html#RSF2v0",
            #   "generation": 1472745315,
            #   "description": "Selected power meters from the RSF 2",
            #   "label": "RSF 2 Version 0",
            #   "record_count": 189101,
            #   "tags": {
            #     "DC.source": "https://skyspark-ops.nrel.gov/proj/nrel",
            #     "DC.description": "Selected power meters from the RSF 2",
            #     "DC.creator": "Brian W Bush <brian.bush@nrel.gov>"
            #   }
            # }

    **API Info**:

        .. http:post:: /models/{model_id}

          :statuscode 200: json
          :statuscode 400: error message
          :statuscode 500: error message
    """
    url = '{}/{}/{}'.format(api_url, root, model_id)
    res = requests.get(url)
    try:
        res.raise_for_status()
        return res.json()
    except:
        log.error('%s - %s', res.status_code, res.text)
        return buildErrorPacket(res)

def list(api_url):
    """Get a set model id.

    Args:
        api_url (string): A url to query

    Returns:
        array: A set of model IDs

    Examples:

        .. code-block:: python

            import cesdspy
            cesdspy.models.list('http://1lv11lamb01.nrel.gov:8091/')

            # Returns:
            # [
            #     "RSF2v0"
            # ]

    **API Info**:

        .. http:post:: /

          :statuscode 200: json
          :statuscode 400: error message
          :statuscode 500: error message
    """
    res = requests.get(api_url)
    try:
        res.raise_for_status()
        return res.json()['models']
    except:
        log.error('%s - %s', res.status_code, res.text)
        return buildErrorPacket(res)

def issue_command(api_url, model_id, body={}):
    """Issue a model command.

    Args:
        api_url (str): A url to query
        model_id (string): A model ID
        body (json): A command to issue

    Returns:
        json: The result of the issued command

    Examples:

        .. code-block:: python

            import cesdspy
            command = {
                "command": "get_model_strategy"
            }
            cesdspy.models.issue_command('http://1lv11lamb01.nrel.gov:8091/', 'RSF2v0', command)

            # Returns:
            # {
            #     "result" : "default strategy"
            # }

    **API Info**:

        .. http:post:: /models/{model_id}/command

            :reqjson string command: one of {"restart", "clear", "set_model_strategy", "get_model_strategy"}
            :reqjson array(any) param: command parameters

            :resjson string result: the result of the issued command

          :statuscode 200: json
          :statuscode 400: error message
          :statuscode 500: error message
    """
    url = '{}/{}/{}/{}'.format(api_url, root, model_id, 'command')
    res = requests.get(url, data=body)
    try:
        res.raise_for_status()
        return res.json()
    except:
        log.error('%s - %s', res.status_code, res.text)
        return buildErrorPacket(res)
