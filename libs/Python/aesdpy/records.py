import requests
import logging
from utils.errors import buildErrorPacket
import json

log = logging.getLogger(__name__)
root = 'models'
"""The root endpoint"""
endpoint = 'records'
"""The endpoint for this module"""

def get(api_url, model_id, record_id, query={}):
    """Get a single record

    Args:
        api_url (string): The base url to query
        model_id (string): The model id
        record_id (string): The record id
        query (json): The query params

    Returns:
        json: A record

    Example:

        .. code-block:: python

            import aesdpy.records
            records.get('http://1lv11lamb01.nrel.gov:8091', 'RSF2v0', '1468500823')

            # Returns:
            # {
            #   'variables': [
            #     {
            #       'id': '@1edb6d30-5d812ce6',
            #       'value': -0.7799999713897705
            #     },
            #     {
            #       'id': '@1edb6d30-d9847159',
            #       'value': -33.380001068115234
            #     },
            #     ...
            #   ],
            #   'id': '1468500823'
            # }

    Example:

        .. code-block:: python

            from aesdpy import records
            query = {
                'variables': '@1edb6d30-d9847159, @1edb6d30-fa21e31d'
            }
            records.get('http://1lv11lamb01.nrel.gov:8091', 'RSF2v0', '1468500823', query)

            # Returns:
            # {
            #   'variables': [
            #     {
            #       'id': '@1edb6d30-fa21e31d',
            #       'value': 0.10500000417232513
            #     },
            #     {
            #       'id': '@1edb6d30-d9847159',
            #       'value': -33.380001068115234
            #     }
            #   ],
            #   'id': '1468500823'
            # }

    **API Info**:

        .. http:get:: /models/{model_id}/records

            :resjson array(dict) variables: A list of variables each with 'id' and 'value' keys
            :resjson string id: A record ID

            :statuscode 200: json
            :statuscode 400: error description
            :statuscode 500: error description
    """
    url = '{}/{}/{}/{}/{}'.format(api_url, root, model_id, endpoint, record_id)
    res = requests.get(url, params=query)
    try:
        res.raise_for_status()
        return res.json()
    except:
        log.error('%s - %s', res.status_code, res.text)
        return buildErrorPacket(res)

def list(api_url, model_id, query={}):
    """Get a single record

    Args:
        api_url (string): The base url to query
        model_id (string): The model id
        record_id (string): The record id
        query (json): The query params

    Returns:
        json: A list of records

    Examples:

        .. code-block:: python

            import aesdpy
            query = {
                variables: '@1edb6d30-5d812ce6, @1edb6d30-d9847159, @1edb6d30-9a89271e',
                from: '',
                to: ''
            }
            aesdpy.records.get('http://1lv11lamb01.nrel.gov:8091/', 'RSF2v0', query)

            # Returns:
            # [
            #     {
            #       'variables': [
            #         {
            #           'id': '@1edb6d30-5d812ce6',
            #           'value': -0.7799999713897705
            #         },
            #         {
            #           'id': '@1edb6d30-d9847159',
            #           'value': -29.600000381469727
            #         },
            #         ...
            #       ],
            #       'id': '1468500780'
            #     },
            #     ...
            # ]

    **API Info**:

        .. http:get:: /models/{model_id}/records/{record_id}

            :resjson array(dict) variables: A list of variables each with 'id' and 'value' keys
            :resjson string id: A record ID

            :statuscode 200: json
            :statuscode 400: error description
            :statuscode 500: error description
    """
    url = '{}/{}/{}/{}'.format(api_url, root, model_id, endpoint)
    res = requests.get(url, params=query)
    try:
        res.raise_for_status()
        return res.json()
    except:
        log.error('%s - %s', res.status_code, res.text)
        return buildErrorPacket(res)

def insert(api_url, model_id, body={}):
    """Add a single record

    Args:
        api_url (string): The base url to query
        model_id (string): The model id
        body (json): The record to insert

    Returns:
        json: The result of the insert

    Examples:

        .. code-block:: python

            import aesdpy
            record = {
              'variables': [
                {
                  'id': '@1edb6d30-5d812ce6',
                  'value': -0.7799999713897705
                },
                {
                  'id': '@1edb6d30-d9847159',
                  'value': -29.600000381469727
                },
                ...
              ]
            }
            aesdpy.records.insert('http://1lv11lamb01.nrel.gov:8091/', 'RSF2v0', record)

            # Returns:
            # {
            #     "result": "ok"
            # }

    **API Info**:

        .. http:post:: /models/{model_id}/records

            :reqjson array(dict) variables: A list of variables each with 'id' and 'value' keys
            :reqjson string id: A record ID

            :resjson string result: The result of the insert

            :statuscode 200: json
            :statuscode 400: error description
            :statuscode 500: error description
    """
    url = '{}/{}/{}/{}'.format(api_url, root, model_id, endpoint)
    res = requests.post(url, data=json.dumps(body))
    try:
        res.raise_for_status()
        return res.json()
    except:
        log.error('%s - %s', res.status_code, res.text)
        return buildErrorPacket(res)
