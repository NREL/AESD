import requests
import logging
from utils.errors import buildErrorPacket

log = logging.getLogger(__name__)

def get(api_url):
    """Get information about a CESDS server.

    Args:
        api_url (string): The base url to query

    Returns:
        json: The CESDS server information

    Example:

        .. code-block:: python

            import cesdspy
            cesdspy.servers.get('http://1lv11lamb01.nrel.gov:8091/')

        .. code-block:: javascript
            :caption: **cesdspy.servers.get response**

            {
              "status": {
                "state": "ok",
                "message": "operating normally"
              },
              "server_id": "CESDS Haystack",
              "server_type": "record_server",
              "models": [
                "RSF2v0"
              ],
              "version": 1
            }

    API Info:

        .. http:get:: /

            :>resjson string server_id: the server's id
            :>resjson string server_type: the server type, e.g. "record_server"
            :>resjson string version: the CESDS API version number
            :>resjson array models: a list of the server's model ids
            :>resjson json status: the server's status
    """
    res = requests.get(api_url)
    try:
        res.raise_for_status()
        return res.json()
    except:
        log.error('%s - %s', res.status_code, res.text)
        return buildErrorPacket(res)

def issue_command(api_url, body={}):
    """Issue a server command.

    Args:
        api_url (str): A url to query
        body (json): A command to issue

    Returns:
        json: The result of the issued command

    Examples:

        .. code-block:: python

            import cesdspy
            body = {
                "command": "get_model_strategy"
            }
            cesdspy.servers.issue_command('http://1lv11lamb01.nrel.gov:8091/', body)

            # Returns:
            # {
            #     "result" : "ok"
            # }

    API Info:

        .. http:post:: /command

            :reqjson string command: one of {"restart", "clear", "set_model_strategy", "get_model_strategy"}
            :reqjson array(any) param: command parameters

            :resjson string result: the result of the issued command

          :statuscode 200: json
          :statuscode 400: error message
          :statuscode 500: error message
    """
    url = '{}/{}'.format(api_url, 'command')
    res = requests.post(api_url, data=body)
    try:
        res.raise_for_status()
        return res.json()
    except:
        log.error(' POST %s %s - %s', url, res.status_code, res.text)
        return buildErrorPacket(res)
