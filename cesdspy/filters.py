import requests
import logging
import json
from utils.errors import buildErrorPacket

root = 'models'
endpoint = 'filters'

log = logging.getLogger(__name__)

def list(api_url, model_id, query={}):
    '''Get all filters. Provide tag information in query to search for specific items.

    Args:
        api_url (string): A url to query
        model_id (string): A model ID
        query (json): The query params

    Returns:
        json: A set of filters

    Examples:

        .. code-block:: python
            :caption: cesdspy.filters.list success 1

            import cesdspy
            cesdspy.filters.list('http://1lv11lamb01.nrel.gov:8090', 'f0')

            # Returns:
            # [
            #     {
            #         'meta': {
            #             'color': '#e70608',
            #             'count': 12,
            #             'name': 'nHwpyBTDPNrI',
            #             'filter_id': 'ZWXdQDSRqcVtalK5AOGA2QLx1ip',
            #             'tags': {
            #                 '8jEPi': 'dq',
            #                 '8GZ6': 'lA68'
            #             }
            #         }
            #     },
            #     ...
            # ]

        .. code-block:: python
            :caption: cesdspy.filters.list success 2

            import cesdspy
            cesdspy.filters.list('http://1lv11lamb01.nrel.gov:8090', 'f0', {'g': '"CENj"'})

            # Returns:
            # [
            #     {
            #         'meta': {
            #             'color': '#eb47e1',
            #             'count': 14,
            #             'name': 'Iuvd3',
            #             'filter_id': 'Lcb1LAKgGFfSHztsxjn8rB4',
            #             'tags': {
            #                 'g': 'CENj',
            #                 'GU': '2gYeo',
            #                 'zq': 'WoF',
            #                 'wCC6': 'I'
            #             }
            #         }
            #     }
            # ]

        .. code-block:: python
            :caption: cesdspy.filters.list failure 1

            import cesdspy
            # note: 'CENj' not '"CENj"'
            cesdspy.filters.list('http://1lv11lamb01.nrel.gov:8090', 'f0', {'g': 'CENj'})

            # Returns:
            # {
            #     'error': {
            #         'status': 400,
            #         'message': 'Bad Request',
            #         'description': 'Error in $: Failed reading: not a valid json value'
            #     }
            # }

        .. code-block:: python
            :caption: cesdspy.filters.list failure 2

            import cesdspy
            cesdspy.filters.list('http://1lv11lamb01.nrel.gov:8090', 'f0', {'g': ''})

            # Returns:
            # {
            #     'error': {
            #         'status': 400,
            #         'message': 'Bad Request',
            #         'description': 'Error in $: not enough input'
            #     }
            # }

    **API Info**:

        .. http:get:: /models/{model_id}/filters

            :param string tag_id: a tag ID

            :resjson dict expr:
            :resjson dict expr['not']:
            :resjson array(dict) expr['union']:
            :resjson array(dict) expr['isect']:
            :resjson string expr['var']: a variable id
            :resjson string|double expr['value']: a variable value
            :resjson string|double expr['set']: a variable value
            :resjson array(string|double) expr['interval']: a variable value
            :resjson dict meta:
            :resjson dict meta['filter_id']: a unique filter ID
            :resjson string meta['name']: a filter name
            :resjson string meta['count']: number of participating records
            :resjson string meta['color']: hex color, e.g. '#ffffff'
            :resjson dict meta['tags']: a set of tags
            :resjson string(any) meta['tags'][tag_id]: a tag description

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

def get(api_url, model_id, filter_id):
    '''Get a specific filter.

    Args:
        api_url (string): A url to query
        model_id (string): A model ID
        filter_id (string): A filter ID

    Returns:
        json: A filter

    Example:

        .. code-block:: python
            :caption: cesdspy.filters.get success

            import cesdspy
            cesdspy.filters.get('http://1lv11lamb01.nrel.gov:8090', 'f0', '4Gc8vhetIddL0zV8aSB')

            # Returns:
            # {
            #     'expr': {
            #         'var': 'Pw0Z',
            #         'interval': [
            #             -2.3064296535256728,
            #             1.116740032936022
            #         ]
            #     },
            #     'meta': {
            #         'color': '#9e6975',
            #         'count': 23,
            #         'name': 'NkEk8k9n6R3f0uKDeF3AnTfz7',
            #         'filter_id': '4Gc8vhetIddL0zV8aSB',
            #         'tags': {
            #             'KZpj': 1.3138264738465626
            #         }
            #     }
            # }

        .. code-block:: python
            :caption: cesdspy.filters.get failure 1

            import cesdspy
            cesdspy.filters.get('http://1lv11lamb01.nrel.gov:8090', 'f0', 'abcd')

            # Response:
            # {
            #     'error': {
            #         'status': 400,
            #         'message': 'Bad Request',
            #         'description': 'filter not found'
            #     }
            # }

    **API Info**:

        .. http:get:: /models/{model_id}/filters/{filter_id}

            :resjson dict expr:
            :resjson dict expr['not']:
            :resjson array(dict) expr['union']:
            :resjson array(dict) expr['isect']:
            :resjson string expr['var']: a variable id
            :resjson string|double expr['value']: a variable value
            :resjson string|double expr['set']: a variable value
            :resjson array(string|double) expr['interval']: a variable value
            :resjson dict meta:
            :resjson dict meta['filter_id']: a unique filter ID
            :resjson string meta['name']: a filter name
            :resjson string meta['count']: number of participating records
            :resjson string meta['color']: hex color, e.g. '#ffffff'
            :resjson dict meta['tags']: a set of tags
            :resjson string(any) meta['tags'][tag_id]: a tag description

            :statuscode 200: json
            :statuscode 400: error description
            :statuscode 500: error description
    '''
    url = '{}/{}/{}/{}/{}'.format(api_url, root, model_id, endpoint, filter_id)
    res = requests.get(url)
    try:
        res.raise_for_status()
        return res.json()
    except:
        log.error('%s - %s', res.status_code, res.text)
        return buildErrorPacket(res)

def add(api_url, model_id, body={}):
    '''Insert a filter.

    Args:
        api_url (string): A url to query
        model_id (string): A model ID
        body (json): A filter to save

    Returns:
        json: A filter

    Examples:

        .. code-block:: python
            :caption: cesdspy.filters.add success

            import cesdspy
            filter = {
                'expr': {
                    'var': 'Pw0Z',
                    'interval': [
                        -2.3064296535256728,
                        1.116740032936022
                    ]
                },
                {
                    'meta': {
                        'name': 'ex1',
                        'count': 1,
                        'color': '#ffffff',
                        'tags': {
                            'DC.Creator': 'cesdspy'
                        }
                    }
                }
            }
            cesdspy.filters.add('http://1lv11lamb01.nrel.gov:8090', 'f0', filter)

            # Returns:
            # {
            #     'expr': {
            #         'var': 'Pw0Z',
            #         'interval': [
            #             -2.3064296535256728,
            #             1.116740032936022
            #         ]
            #     },
            #     'meta': {
            #         'color': '#ffffff',
            #         'count': 1,
            #         'name': 'ex1',
            #         'filter_id': '5Oqn',
            #         'tags': {
            #             'DC.Creator': 'cesdspy'
            #         }
            #     }
            # }

        .. code-block:: python
            :caption: cesdspy.filters.add failure 1

            import cesdspy
            filter = {
                {
                    'meta': {
                        'count': 1,
                        'color': '#ffffff',
                        'tags': {
                            'DC.Creator': 'cesdspy'
                        }
                    }
                }
            }
            cesdspy.filters.add('http://1lv11lamb01.nrel.gov:8090', 'f0', filter)

            # Returns:
            #
            # {
            #     'error': {
            #         'status': 400,
            #         'message': 'Bad Request',
            #         'description': 'illegal JSON: Error in $: key 'name' not present'
            #     }
            # }

        .. code-block:: python
            :caption: cesdspy.filters.add failure 2

            import cesdspy
            filter = {
                'meta': {
                    'name': 'ex1',
                    'filter_id': 'ZWXdQDSRqcVtalK5AOGA2QLx1ip',
                }
            }
            cesdspy.filters.add('http://1lv11lamb01.nrel.gov:8090', 'f0', filter)

            # Returns:
            # {
            #     'error': {
            #         'status': 400,
            #         'message': 'Bad Request',
            #         'description': 'duplicate filter identifier'
            #     }
            # }

    **API Info**:

        .. http:post:: /models/{model_id}/filters

            :reqjson dict expr:
            :reqjson dict expr['not']:
            :reqjson array(dict) expr['union']:
            :reqjson array(dict) expr['isect']:
            :reqjson string expr['var']: a variable id
            :reqjson string|double expr['value']: a variable value
            :reqjson string|double expr['set']: a variable value
            :reqjson array(string|double) expr['interval']: a variable value
            :reqjson dict meta:
            :reqjson dict meta['filter_id']: a unique filter ID
            :reqjson string meta['name']: a filter name
            :reqjson string meta['count']: number of participating records
            :reqjson string meta['color']: hex color, e.g. '#ffffff'
            :reqjson dict meta['tags']: a set of tags
            :reqjson string(any) meta['tags'][tag_id]: a tag description

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

def remove(api_url, model_id, filter_id):
    '''Delete a filter.

    Args:
        api_url (string): A url to query
        model_id (string): A model ID
        filter_id (string): A filter ID

    Returns:
        json: result

    Examples:

        .. code-block:: python
            :caption: cesdspy.filters.remove success

            import cesdspy
            cesdspy.filters.remove('http://1lv11lamb01.nrel.gov:8090', 'f0', 'eUIbEM8')

            # Returns:
            # {
            #     'result': 'ok'
            # }

        .. code-block:: python
            :caption: cesdspy.filters.remove failure

            import cesdspy
            cesdspy.filters.remove('http://1lv11lamb01.nrel.gov:8090', 'f0', 'eUIbEM8')

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

        .. http:delete:: /models/{model_id}/filters/{filter_id}

            :resjson string result: the result of delete

            :statuscode 200: json
            :statuscode 400: error description
            :statuscode 500: error description
    '''
    url = '{}/{}/{}/{}/{}'.format(api_url, root, model_id, endpoint, filter_id)
    res = requests.delete(url)
    try:
        res.raise_for_status()
        return res.json()
    except:
        log.error('%s - %s', res.status_code, res.text)
        return buildErrorPacket(res)
