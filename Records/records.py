"""
Design Studio Python Records API

Created by: Michael Rossol Feb. 2017
"""
import asyncio
import records_def_4_pb2 as proto
import websockets

currentID = 0

__all__ = ['next_ID', ]


def next_ID():
    """
    Creates new ID from global currentID counter
    Parameters
    ----------

    Returns
    -------
    currentID : 'int'
        New ID
    """
    global currentID
    currentID += 1
    return currentID


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
                raise Exception(response.error)
            else:
                responses.append(response)

            if response.next_chunk_id < 1:
                break

    return responses


async def send_request(url, request):
    """
    Send request to server and compile response
    Parameters
    ----------
    url : 'string'
        Server url
    request : 'proto.request'
        proto request message

    Returns
    -------
    response : 'list'
        List of responses from the server, each response is a proto message
    """
    async with websockets.connect(url) as websocket:
        await websocket.send(request.SerializeToString())

        request_id = request.id.value
        response = await on_response(websocket, request_id)

    return response


class CESDS(object):
    def __init__(self, server_url):
        """
        CESDS class instance creates an asyncio event loop
        Parameters
        ----------
        path_in : 'sting'
            Path to WRF netCDF file.

        Returns
        ---------
        self.path_in : path to WRF netCDF file
        self.dataset : netCDF dataset instance
        self.variables : list of WRF variables
        self.dimensions : list of dimensions for netCDF file
        """
        self.url = server_url
        self.event_loop = asyncio.get_event_loop()

    def __repr__(self):
        print('{n} connecting to {s}'
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
        self.disconnect

        if type is not None:
            raise

    @property
    def disconnect(self):
        """
        Closes event_loop
        Parameters
        ----------

        Returns
        ---------
        """
        self.event_loop.close()
