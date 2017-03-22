"""
Package for models request

Created by: Michael Rossol Feb. 2017
"""
from .errors import ProtoError
import records_def_4_pb2 as proto

__all__ = ['request_bookmark_meta', 'from_bookmark_meta',
           'from_bookmark_meta_list', 'handle_bookmark_response']


def request_bookmark_meta(model_id, bookmark_id, request_id, version=4):
    """
    Create request for bookmark bookmark_meta
    Parameters
    ----------
    model_id : 'string'
        Id of model for which to requst bookmark_meta
    bookmark_id : 'string'
        Id of bookmark for which to request models_metadata
        if None request all bookmarks
    request_id : 'int'
        Unique request id
    version : 'int'
        Google protobuf version, default = 4

    Returns
    -------
    request : 'proto.Request'
        proto Request message for models_metadata
    """
    request = proto.Request()
    request.version = version
    request.id.value = request_id
    bookmark_request = request.bookmark_meta
    bookmark_request.model_id = model_id
    if bookmark_id is not None:
        bookmark_request.bookmark_id.value = bookmark_id
    else:
        bookmark_request.bookmark_id.value = ''
    return request


def from_bookmark_meta(bookmark_meta):
    """
    Unpacks proto BookmarkMeta message to dictionary
    Parameters
    ----------
    bookmark_meta : 'proto.BookmarkMeta'
        proto BookmarkMeta message from BookmarkMetaList

    Returns
    -------
    'dict'
        Dictionary containing the bookmark's metadata
    """
    bookmark_dict = {'id': bookmark_meta.bookmark_id,
                     'name': bookmark_meta.bookmark_name}
    content = bookmark_meta.WhichOneof('content')

    if content == 'interval':
        interval = bookmark_meta.interval
        bookmark_dict['interval'] = (interval.first_record,
                                     interval.last_record)
    elif content == 'set':
        bookmark_dict['set'] = bookmark_meta.set.record_ids

    elif content == 'filter':
        # INCOMPLETE need parser for FilterExpression
        bookmark_dict['filter'] = bookmark_meta.filter
    else:
        raise ProtoError('Cannot parce BookmarkMeta content!')

    return bookmark_dict


def from_bookmark_meta_list(bookmarks):
    """
    Unpacks proto BookmarkMetaList message to list of dictionaries
    Parameters
    ----------
    bookmarks : 'proto.BookmarkMetaList'
        proto BookmarkMeta message from BookmarkMetaList

    Returns
    -------
    'list'
        Dictionary containing the bookmark's metadata
    """
    return list(map(from_bookmark_meta, bookmarks.bookmark_metas))


def handle_bookmark_response(response):
    """
    Extract bookmark metadata from each server response message
    Parameters
    ----------
    response : 'list'
        list of proto Response messages

    Returns
    -------
    bookmark_metadata : 'list'
        List of bookmark's metadata dictionaries for each model in models
    """
    bookmark_metadata = []
    for message in response:
        bookmark_metadata.extend(from_bookmark_meta_list(message.bookmarks))

    return bookmark_metadata


def save_bookmark(model_id, name, content, request_id, version=4):
    """
    Create request for bookmark bookmark_meta
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
    request_id : 'int'
        Unique request id
    version : 'int'
        Google protobuf version, default = 4

    Returns
    -------
    request : 'proto.Request'
        proto Request message for models_metadata
    """
    # Does not get a response from server
    request = proto.Request()
    request.version = version
    request.id.value = request_id
    new_bookmark = request.save_bookmark.new_bookmark
    new_bookmark.bookmark_name = name
    if isinstance(content, tuple):
        assert len(content) == 2, 'Interval can only have a first and \
last record'
        new_bookmark.interval.first_record = content[0]
        new_bookmark.interval.last_record = content[1]
    elif isinstance(content, list):
        new_bookmark.set.record_ids.extent(content)
    else:
        # INCOMPLETE need filter expression parsers to pass filter
        raise ProtoError('Cannot parse bookmark content!')

    return request
