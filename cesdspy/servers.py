import requests
import logging

log = logging.getLogger(__name__)

def get(api_url):
    """Get information about a CESDS server

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

    Note:

        .. http:get:: /

            :>json string server_id: the server's id
            :>json string server_type: the server type, e.g. "record_server"
            :>json string version: the CESDS API version number
            :>json array models: a list of the server's model ids
            :>json json status: the server's status
    """
    res = requests.get(api_url)
    try:
        res.raise_for_status()
        return res.json()
    except:
        log.error('%s - %s', res.status_code, res.text)

def issue_command(api_url, body={}):
    """Issue a command to a server

    Args:
        api_url (str): The base url to query
        body (json): The command to issue

    Returns:
        json: The result of the issued command

    Example:

        .. code-block:: python

            import cesdspy
            body = {
                "command": "get_model_strategy"
            }
            cesdspy.servers.issue_command('http://1lv11lamb01.nrel.gov:8091/', body)

        .. code-block:: javascript
            :caption: **cesdspy.servers.issue_command response**

            {
                "result" : "TODO"
            }

    Note:

        .. http:post:: /command

            :form string command: one of {"restart", "clear", "set_model_strategy", "get_model_strategy"}
            :form array(any) param: command parameters

            :json string result: the result of the issued command

            **Example request**:

            .. sourcecode:: http

                GET /command HTTP/1.1
                Host: http://1lv11lamb01.nrel.gov:8091
                Accept: application/json

            **Example response**:

            .. sourcecode:: http

                HTTP/1.1 200 OK
                Content-Type: text/json

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
