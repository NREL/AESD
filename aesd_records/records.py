"""
Design Studio Python Records API

Created by: Michael Rossol Feb. 2017
"""
import asyncio
import async_timeout
from .bookmarks import (request_bookmark_meta, handle_bookmark_response,
                        save_bookmark)
from .data import (request_records_data, handle_data_response, request_work)
from .errors import TimeoutError, ProtoError
from .models import (request_model_metadata, handle_models_response)
from . import records_def_4_pb2 as proto
import websockets

__all__ = ['on_response', 'send_request', 'Records']


async def on_response(websocket, request_id):
    """
    Extracts responses to request with request_id
    Parameters
    ----------
    websocket : 'websocket instance'
        websocket connection
    request_id : 'int'
        Id of request sent to server
        Responses must have the same id

    Returns
    -------
    responses : 'list'
        List of responses from the server, each response is a proto message
    """
    responses = []

    while True:
        response = proto.Response()
        response.ParseFromString(await websocket.recv())
        if response.id.value == request_id:
            response_type = response.WhichOneof('type')
            if response_type == 'error':
                raise ProtoError(response.error)
            else:
                responses.append(response)

            if response.next_chunk_id < 1:
                break

    return responses


async def send_request(url, request, timeout=60):
    """
    Send request to server and compile response
    Parameters
    ----------
    url : 'string'
        Server url
    request : 'proto.request'
        proto request message
    timeout : 'int'
        timeout time for communication with server in sec

    Returns
    -------
    response : 'list'
        List of responses from the server, each response is a proto message
    """
    with async_timeout.timeout(timeout):
        async with websockets.connect(url) as websocket:
            await websocket.send(request.SerializeToString())

            request_id = request.id.value
            response = await on_response(websocket, request_id)

    return response


class Records(object):
    """
    Records class to handle communication with servers
    """
    currentID = 0
    version = 4

    def __init__(self, server_url, timeout=60):
        """
        Records class instance creates an asyncio event loop
        Parameters
        ----------
        server_url : 'string'
            server url
        timeout : 'int'
            timeout time for communication with server in sec

        Returns
        ---------
        self.url : 'string'
            server url
        """
        self.url = server_url
        self.timeout = timeout

    def __repr__(self):
        return ('{n} connected to {s}'
                .format(n=self.__class__.__name__, s=self.url))

    def __enter__(self):
            """
            Enter method to allow use of with
            Parameters
            ----------

            Returns
            ---------
            """
            return self

    def __exit__(self, type, value, traceback):
        """
        Closes event_loop on exit from with
        Parameters
        ----------

        Returns
        ---------
        """
        if type is not None:
            raise

    def new_server(self, server_url):
        """
        Change server url to which websocket will connnect
        Parameters
        ----------
        server_url : 'string'
            server url

        Returns
        ---------
        self.url : 'string'
            server url
        """
        self.url = server_url

    @property
    def next_ID(self):
        """
        update currentID and return new unique request_ID
        Parameters
        ----------

        Returns
        ---------
        """
        self.currentID += 1
        return self.currentID

    def send(self, request):
        """
        Closes event_loop
        Parameters
        ----------
        request : 'proto.request'
            proto request message
        timeout : 'int'
            timeout in seconds for connection

        Returns
        ---------
        response : 'list'
            List of responses from the server, each response is a proto message
        """
        try:
            event_loop = asyncio.new_event_loop()
            asyncio.set_event_loop(event_loop)
            sec = self.timeout
            response = event_loop.run_until_complete(send_request(self.url,
                                                                  request,
                                                                  timeout=sec))
            return response
        except asyncio.TimeoutError:
            raise TimeoutError('Connection timed out after {:} seconds!'
                               .format(sec))
        except Exception:
            raise
        finally:
            event_loop.close()

    def get_model_info(self, model_id):
        """
        Sends request of model metadata and extracts response
        Parameters
        ----------
        model_id : 'string'
            Id of model for which to requst models_metadata
            if None requests all models

        Returns
        -------
        model_info : 'list'|'dict'
            List of model's metadata dictionaries for each model in models or
            dictionary for model_id
        """
        request_id = self.next_ID
        request = request_model_metadata(model_id, request_id,
                                         version=self.version)

        response = self.send(request)

        model_info = handle_models_response(response)

        if len(model_info) == 1:
            return model_info[0]
        else:
            return model_info

    def get_data(self, model_id, max_records=1000, variable_ids=None,
                 bookmark_id=None):
        """
        Sends request of model metadata and extracts response
        Parameters
        ----------
        model_id : 'string'
            Id of model for which to requst records_data
        max_records : 'int'
            Number or records being request (0 will return all records)
        variable_ids : 'list'
            List of variable ids (ints) to be requested
            Will be returned in same order as request
            Default=None, all variables will be returned (order?)
        bookmark_id : 'int'
            Request records_data based on bookmark id

        Returns
        -------
        data : 'pd.DataFrame'
            Concatinated data from each response message
            Variable ids replaced with names from model_info
        """
        request_id = self.next_ID
        request = request_records_data(model_id, request_id,
                                       max_records=max_records,
                                       variable_ids=variable_ids,
                                       bookmark_id=bookmark_id,
                                       version=self.version)

        response = self.send(request)
        data = handle_data_response(response)

        model_info = self.get_model_info(model_id)
        variables = {var['id']: var['name'] for var in model_info['variables']}

        return data.rename(columns=variables)

    def get_bookmark_info(self, model_id, bookmark_id):
        """
        Sends request of model metadata and extracts response
        Parameters
        ----------
        model_id : 'string'
            Id of model for which to requst bookmark_meta
        bookmark_id : 'string'
            Id of bookmark for which to request models_metadata
            if None request all bookmarks

        Returns
        -------
        model_info : 'list'|'dict'
            List of model's metadata dictionaries for each model in models or
            dictionary for model_id
        """
        request_id = self.next_ID
        request = request_bookmark_meta(model_id, bookmark_id, request_id,
                                        version=self.version)

        response = self.send(request)

        bookmark_info = handle_bookmark_response(response)

        if len(bookmark_info) == 1:
            return bookmark_info[0]
        else:
            return bookmark_info

    def save_bookmark(self, model_id, name, content):
        """
        Sends request to save new bookmark
        Parameters
        ----------
        model_id : 'string'
            Id of model for which to requst bookmark_meta
        name : 'string'
            Name for new bookmark
        content : 'list'|'tuple'
            Contents of bookmark
            list is a bookmark set
            tuple is a bookmark interval

        Returns
        -------
        model_info : 'list'|'dict'
            List of model's metadata dictionaries for each model in models or
            dictionary for model_id
        """
        request_id = self.next_ID
        request = save_bookmark(model_id, name, content, request_id,
                                version=self.version)

        response = self.send(request)

        bookmark_info = handle_bookmark_response(response)

        if len(bookmark_info) == 1:
            return bookmark_info[0]
        else:
            return bookmark_info

    def do_work(self, model_id, inputs):
        """
        Sends request of model metadata and extracts response
        Parameters
        ----------
        model_id : 'string'
            Id of model for which to requst records_data
        inputs : 'dict'
            Dictionary of {var_id: value} pairs

        Returns
        -------
        data : 'pd.DataFrame'
            Concatinated data from each response message
            Variable ids replaced with names from model_info
        """
        request_id = self.next_ID
        request = request_work(model_id, inputs, request_id,
                               version=self.version)

        response = self.send(request)
        data = handle_data_response(response)

        model_info = self.get_model_info(model_id)
        variables = {var['id']: var['name'] for var in model_info['variables']}

        return data.rename(columns=variables)
