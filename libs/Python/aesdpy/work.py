import requests
import logging
import json
from utils.errors import buildErrorPacket

root = 'models'
endpoint = 'work'

log = logging.getLogger(__name__)

def list(api_url, model_id, query={}):
    '''Get all work. Provide tag information in query to search for specific items.

    Args:
        api_url (string): A url to query
        model_id (string): A model ID
        query (json): The query params

    Returns:
        json: A set of work

    Examples:

        .. code-block:: python
            :caption: aesdpy.work.list success 1

            import aesdpy
            aesdpy.work.list('http://1lv11lamb01.nrel.gov:8090', 'XXp')

            # Returns:
            # [
            #     {
            #         "status": "success",
            #         "work_id": "OGXnJpuRgSvU7KpTXpXY06",
            #         "result_id": "Il81Na1b665QiXyF"
            #     },
            #     ...
            # ]

        .. code-block:: python
            :caption: aesdpy.work.list success 2

            import aesdpy
            aesdpy.work.list('http://1lv11lamb01.nrel.gov:8090', 'f0', {'status': 'failed'})

            # Returns:
            # [
            #     {
            #         "status": "failed",
            #         "work_id": "pRXld2YbWfN1c8XU4d0tVp5Q",
            #         "additional": "0uLKujNXJwh6qV8LAqQTrCjQs0Dd"
            #     },
            #     ...
            # ]

    **API Info**:

        .. http:get:: /models/{model_id}/work

            :param string status: a tag ID
            :param string from: start generation
            :param string to: end generation


            :resjson string status: work status
            :resjson string work_id: a work ID
            :resjson additional: a additional ID

            :statuscode 200: json
            :statuscode 400: error description
            :statuscode 500: error description
    '''
    url = '{}/{}/{}/{}'.format(api_url, root, model_id, endpoint)
    res = requests.get(url, params=query)
    try:
        res.raise_for_status()
        return res.json()
    except:
        log.error('%s - %s', res.status_code, res.text)
        return buildErrorPacket(res)

def get(api_url, model_id, work_id):
    '''Get a specific work.

    Args:
        api_url (string): A url to query
        model_id (string): A model ID
        work_id (string): A work ID

    Returns:
        json: A work

    Example:

        .. code-block:: python
            :caption: aesdpy.work.get success

            import aesdpy
            aesdpy.work.get('http://1lv11lamb01.nrel.gov:8090', 'f0', 'X7Rsg')

            # Returns:
            # {
            #     "status": "success",
            #     "work_id": "X7Rsg",
            #     "result_id": "NIKm9EnR3hjvfV"
            # }

        .. code-block:: python
            :caption: aesdpy.work.get failure 1

            import aesdpy
            aesdpy.work.get('http://1lv11lamb01.nrel.gov:8090', 'f0', 'abcd')

            # Response:
            # {
            #     'error': {
            #         'status': 400,
            #         'message': 'Bad Request',
            #         'description': 'work not found'
            #     }
            # }

    **API Info**:

        .. http:get:: /models/{model_id}/work/{work_id}

            :resjson string status: work status
            :resjson string work_id: a work ID
            :resjson additional: a additional ID

            :statuscode 200: json
            :statuscode 400: error description
            :statuscode 500: error description
    '''
    url = '{}/{}/{}/{}/{}'.format(api_url, root, model_id, endpoint, work_id)
    res = requests.get(url)
    try:
        res.raise_for_status()
        return res.json()
    except:
        log.error('%s - %s', res.status_code, res.text)
        return buildErrorPacket(res)

def issue(api_url, model_id, body={}):
    '''Issue work.

    Args:
        api_url (string): A url to query
        model_id (string): A model ID
        body (json): Work to issue

    Returns:
        json: work submission result

    Examples:

        .. code-block:: python
            :caption: aesdpy.work.issue success

            import aesdpy
            work = {
                'explicit': [
                    {
                        'id': 'Pw0Z'
                        'value': 2.0
                    }
                ],
                'random': [
                    'nZ'
                ],
                'timeout': 60,
                'priority': 0
            }
            aesdpy.work.issue('http://1lv11lamb01.nrel.gov:8090', 'XXp', work)

            # Returns:
            # {
            #     "work_id": "abc",
            #     "generation": 42,
            #     "estimated_completion": 123
            # }

    **API Info**:

        .. http:post:: /models/{model_id}/work

            :reqjson array(dict) explicit:
            :reqjson array(string) random:
            :reqjson int timeout:
            :reqjson int priority:

            :statuscode 200: json
            :statuscode 400: error description
            :statuscode 500: error description
    '''
    url = '{}/{}/{}/{}'.format(api_url, root, model_id, endpoint)
    res = requests.post(url, data=json.dumps(body))
    try:
        res.raise_for_status()
        return res.json()
    except:
        log.error('%s - %s', res.status_code, res.text)
        return buildErrorPacket(res)

def cancel(api_url, model_id, work_id):
    '''Cancel work.

    Args:
        api_url (string): A url to query
        model_id (string): A model ID
        work_id (string): A work ID

    Returns:
        json: result

    Examples:

        .. code-block:: python
            :caption: aesdpy.work.remove success

            import aesdpy
            aesdpy.work.cancel('http://1lv11lamb01.nrel.gov:8090', 'f0', 'abc')

            # Returns:
            # {
            #     'result': 'ok'
            # }

        .. code-block:: python
            :caption: aesdpy.work.cancel failure

            import aesdpy
            aesdpy.work.cancel('http://1lv11lamb01.nrel.gov:8090', 'f0', 'abc')

            # Returns:
            #
            # {
            #     'error': {
            #         'status': 400,
            #         'message': 'Bad Request',
            #         'description': 'not supported'
            #     }
            # }

    **API Info**:

        .. http:delete:: /models/{model_id}/work/{work}

            :resjson string result: the result of delete

            :statuscode 200: json
            :statuscode 400: error description
            :statuscode 500: error description
    '''
    url = '{}/{}/{}/{}/{}'.format(api_url, root, model_id, endpoint, work_id)
    res = requests.delete(url)
    try:
        res.raise_for_status()
        return res.json()
    except:
        log.error('%s - %s', res.status_code, res.text)
        return buildErrorPacket(res)
