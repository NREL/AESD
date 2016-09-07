import requests
import logging
import json
from utils.errors import buildErrorPacket

root = 'models'
endpoint = 'bookmarks'

log = logging.getLogger(__name__)

def list(api_url, model_id, query={}):
    '''Get all bookmarks. Provide tag information in query to search for specific items.

    Args:
        api_url (string): A url to query
        model_id (string): A model ID
        query (json): The query params

    Returns:
        json: A set of bookmarks

    Examples:

        .. code-block:: python
            :caption: cesdspy.bookmarks.list success 1

            import cesdspy
            cesdspy.bookmarks.list('http://1lv11lamb01.nrel.gov:8090', 'XXp')

            # Returns:
            # [
            #     {
            #         "meta": {
            #             "color": "#408422",
            #             "count": 1,
            #             "name": "6ZSG4iEuzxxO2V6G1xZJ",
            #             "bookmark_id": "jftCe0Enb",
            #             "tags": {
            #                 "w": "GQ",
            #                 "5vFit": "i8",
            #                 "o": -1.1554219144162807,
            #                 "c": 3.980237195769979
            #             }
            #         }
            #     },
            #     ...
            # ]

        .. code-block:: python
            :caption: cesdspy.bookmarks.list success 2

            import cesdspy
            cesdspy.bookmarks.list('http://1lv11lamb01.nrel.gov:8090', 'f0', {'lPqy': '5.0250953446542095'})

            # Returns:
            # [
            #     {
            #         "meta": {
            #             "count": 3,
            #             "name": "t0WcvWrXbLA",
            #             "bookmark_id": "GQnQa8KWanDNjdGUvSXwZ9",
            #             "tags": {
            #                 "lPqy": 5.0250953446542095
            #             }
            #         }
            #     }
            # ]

        .. code-block:: python
            :caption: cesdspy.bookmarks.list failure

            import cesdspy
            cesdspy.bookmarks.list('http://1lv11lamb01.nrel.gov:8090', 'f0', {'lPqy': ''})

            # Returns:
            # {
            #     'error': {
            #         'status': 400,
            #         'message': 'Bad Request',
            #         'description': 'Error in $: not enough input'
            #     }
            # }

        **API Info**:

            .. http:get:: /models/{model_id}/bookmarks

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
                :resjson dict meta['bookmark_id']: a unique bookmark ID
                :resjson string meta['name']: a bookmark name
                :resjson string meta['count']: number of participating records
                :resjson string meta['color']: hex color, e.g. '#ffffff'
                :resjson dict meta['tags']: a set of tags
                :resjson string(any) meta['tags'][tag_id]: a tag description

                :statuscode 200: json
                :statuscode 400: error description
                :statuscode 500: error description
    '''
    url = '{}/{}/{}/{}'.format(api_url, root, model_id, endpoint)
    print(query)
    res = requests.get(url, params=query)
    print(res.url)
    try:
        res.raise_for_status()
        return res.json()
    except:
        log.error('%s - %s', res.status_code, res.text)
        return buildErrorPacket(res)

def get(api_url, model_id, bookmark_id):
    '''Get a specific bookmark.

    Args:
        api_url (string): A url to query
        model_id (string): A model ID
        bookmark_id (string): A bookmark ID

    Returns:
        json: A bookmark

    Example:

        .. code-block:: python
            :caption: cesdspy.bookmarks.get success

            import cesdspy
            cesdspy.bookmarks.get('http://1lv11lamb01.nrel.gov:8090', 'f0', '68xQVjByK')

            # Returns:
            # {
            #     "record_ids": [
            #         "Yv9CAD",
            #         "NIKm9EnR3hjvfV"
            #     ],
            #     "meta": {
            #         "color": "#0b1bfb",
            #         "count": 2,
            #         "name": "edRU6O3q1UmeLzwFu2wa",
            #         "bookmark_id": "68xQVjByK",
            #         "tags": {
            #             "2z6L9C": "nJ",
            #             "n": -1
            #         }
            #     }
            # }

        .. code-block:: python
            :caption: cesdspy.bookmarks.get failure 1

            import cesdspy
            cesdspy.bookmarks.get('http://1lv11lamb01.nrel.gov:8090', 'f0', 'abcd')

            # Response:
            # {
            #     'error': {
            #         'status': 400,
            #         'message': 'Bad Request',
            #         'description': 'bookmark not found'
            #     }
            # }

        **API Info**:

            .. http:get:: /models/{model_id}/bookmarks/{bookmark_id}

                :resjson dict expr:
                :resjson dict expr['not']:
                :resjson array(dict) expr['union']:
                :resjson array(dict) expr['isect']:
                :resjson string expr['var']: a variable id
                :resjson string|double expr['value']: a variable value
                :resjson string|double expr['set']: a variable value
                :resjson array(string|double) expr['interval']: a variable value
                :resjson dict meta:
                :resjson dict meta['bookmark_id']: a unique bookmark ID
                :resjson string meta['name']: a bookmark name
                :resjson string meta['count']: number of participating records
                :resjson string meta['color']: hex color, e.g. '#ffffff'
                :resjson dict meta['tags']: a set of tags
                :resjson string(any) meta['tags'][tag_id]: a tag description

                :statuscode 200: json
                :statuscode 400: error description
                :statuscode 500: error description
    '''
    url = '{}/{}/{}/{}/{}'.format(api_url, root, model_id, endpoint, bookmark_id)
    res = requests.get(url)
    try:
        res.raise_for_status()
        return res.json()
    except:
        log.error('%s - %s', res.status_code, res.text)
        return buildErrorPacket(res)

def add(api_url, model_id, body={}):
    '''Insert a bookmark.

    Args:
        api_url (string): A url to query
        model_id (string): A model ID
        body (json): A bookmark to save

    Returns:
        json: A bookmark

    Examples:

        .. code-block:: python
            :caption: cesdspy.bookmarks.add success

            import cesdspy
            bookmark = {
                'meta': {
                    'name': 'ex1',
                    'count': 1,
                    'color': '#ffffff',
                    'tags': {
                        'DC.Creator': 'cesdspy'
                    }
                },
                'record_ids': ['Il81Na1b665QiXyF']
            }
            cesdspy.bookmarks.add('http://1lv11lamb01.nrel.gov:8090', 'XXp', bookmark)

            # Returns:
            # {
            #     "record_ids": [
            #         "Il81Na1b665QiXyF"
            #     ],
            #     "meta": {
            #         "color": "#ffffff",
            #         "count": 1,
            #         "name": "ex1",
            #         "bookmark_id": "rM9m",
            #         "tags": {
            #             "DC.Creator": "cesdspy"
            #         }
            #     }
            # }

        .. code-block:: python
            :caption: cesdspy.bookmarks.add failure 1

            import cesdspy
            bookmark = {
                'meta': {
                    'count': 1,
                    'color': '#ffffff',
                    'tags': {
                        'DC.Creator': 'cesdspy'
                    }
                },
                'record_ids': ['Il81Na1b665QiXyF']
            }
            cesdspy.bookmarks.add('http://1lv11lamb01.nrel.gov:8090', 'XXp', bookmark)

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
            :caption: cesdspy.bookmarks.add failure 2

            import cesdspy
            bookmark = {
                'meta': {
                    'name': 'ex1',
                    'count': 1,
                    'color': '#ffffff',
                    'tags': {
                        'DC.Creator': 'cesdspy'
                    }
                },
                'record_ids': []
            }
            cesdspy.bookmarks.add('http://1lv11lamb01.nrel.gov:8090', 'f0', bookmark)

            # Returns:
            #
            # {
            #     'error': {
            #         'status': 400,
            #         'message': 'Bad Request',
            #         'description': 'no record identifiers in bookmark'
            #     }
            # }

        .. code-block:: python
            :caption: cesdspy.bookmarks.add failure 3

            import cesdspy
            bookmark = {
                'meta': {
                    'name': 'ex1',
                    'count': 1,
                    'color': '#ffffff',
                    'tags': {
                        'DC.Creator': 'cesdspy'
                    }
                },
                'record_ids': ['Il81Na1b665QiXyF']
            }
            # Note: model 'f0' has no record with id 'Il81Na1b665QiXyF'
            cesdspy.bookmarks.add('http://1lv11lamb01.nrel.gov:8090', 'f0', bookmark)

            # Returns:
            #
            # {
            #     'error': {
            #         'status': 400,
            #         'message': 'Bad Request',
            #         'description': 'invalid record identifiers'
            #     }
            # }

        .. code-block:: python
            :caption: cesdspy.bookmarks.add failure 4

            import cesdspy
            bookmark = {
                'meta': {
                    'bookmark_id': 'rM9m',
                    'count': 1,
                    'color': '#ffffff',
                    'tags': {
                        'DC.Creator': 'cesdspy'
                    }
                },
                'record_ids': ['Il81Na1b665QiXyF']
            }
            cesdspy.bookmarks.add('http://1lv11lamb01.nrel.gov:8090', 'f0', bookmark)

            # Returns:
            # {
            #     'error': {
            #         'status': 400,
            #         'message': 'Bad Request',
            #         'description': 'duplicate bookmark identifier'
            #     }
            # }

        **API Info**:

            .. http:post:: /models/{model_id}/bookmarks

                :reqjson dict expr:
                :reqjson dict expr['not']:
                :reqjson array(dict) expr['union']:
                :reqjson array(dict) expr['isect']:
                :reqjson string expr['var']: a variable id
                :reqjson string|double expr['value']: a variable value
                :reqjson string|double expr['set']: a variable value
                :reqjson array(string|double) expr['interval']: a variable value
                :reqjson dict meta:
                :reqjson dict meta['bookmark_id']: a unique bookmark ID
                :reqjson string meta['name']: a bookmark name
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

def remove(api_url, model_id, bookmark_id):
    '''Delete a bookmark.

    Args:
        api_url (string): A url to query
        model_id (string): A model ID
        bookmark_id (string): A bookmark ID

    Returns:
        json: result

    Examples:

        .. code-block:: python
            :caption: cesdspy.bookmarks.remove success

            import cesdspy
            cesdspy.bookmarks.remove('http://1lv11lamb01.nrel.gov:8090', 'f0', 'eUIbEM8')

            # Returns:
            # {
            #     'result': 'ok'
            # }

        .. code-block:: python
            :caption: cesdspy.bookmarks.remove failure

            import cesdspy
            cesdspy.bookmarks.remove('http://1lv11lamb01.nrel.gov:8090', 'f0', 'eUIbEM8')

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

            .. http:delete:: /models/{model_id}/bookmarks/{bookmark_id}

                :resjson string result: the result of delete

                :statuscode 200: json
                :statuscode 400: error description
                :statuscode 500: error description
    '''
    url = '{}/{}/{}/{}/{}'.format(api_url, root, model_id, endpoint, bookmark_id)
    res = requests.delete(url)
    try:
        res.raise_for_status()
        return res.json()
    except:
        log.error('%s - %s', res.status_code, res.text)
        return buildErrorPacket(res)
