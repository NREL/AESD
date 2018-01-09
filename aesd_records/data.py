"""
Package for data requests

Created by: Michael Rossol Feb. 2017
"""
from .errors import ProtoError
import numpy as np
import pandas as pd
from . import records_def_4_pb2 as proto


__all__ = ['request_records_data', 'from_record_data',
           'from_record_list', 'from_record', 'from_value',
           'from_record_table', 'from_list', 'empty_data_chunk',
           'handle_data_response', 'request_work', 'set_var_value']


def request_records_data(model_id, request_id, max_records=1000,
                         variable_ids=None, bookmark_id=None, version=4):
    """
    Create request for records data

    Parameters
    ----------
    model_id : 'string'
        Id of model for which to requst records_data
    request_id : 'int'
        Unique request id
    max_records : 'int'
        Number or records being request
        Default=0, will return all records
    variable_ids : 'list'
        List of variable ids (ints) to be requested
        Will be returned in same order as request
        Default=None, all variables will be returned (order?)
    bookmark_id : 'int'
        Request records_data based on bookmark id
    version : 'int'
        Google protobuf version, default = 4

    Returns
    -------
    request : 'proto.Request'
        proto Request message for records_data
    """
    request = proto.Request()
    request.version = version
    request.id.value = request_id
    data_request = request.records_data
    data_request.model_id = model_id
    data_request.max_records = max_records
    if variable_ids is not None:
        if isinstance(variable_ids, list):
            data_request.var_ids.extend(variable_ids)
        else:
            data_request.var_ids.append(variable_ids)
    if bookmark_id is not None:
        data_request.bookmark_id = bookmark_id

    return request


def from_record_data(record_data):
    """
    Create request for records data

    Parameters
    ----------
    records_data : 'proto.data'
        proto data message from Response type

    Returns
    -------
    'ndarray' or 'pandas.DataFrame'
        Return records_data as either and ndarray or pd.DataFrame
    """
    style = record_data.WhichOneof('style')
    if style == 'list':
        return from_record_list(record_data.list)
    elif style == 'table':
        return from_record_table(record_data.table)
    else:
        raise ProtoError('Cannot parce RecordData style!')


def from_record_list(record_list):
    """
    Extract record data from RecordList

    Parameters
    ----------
    record_list : 'proto.RecordList'
        proto RecordList message from RecordData style

    Returns
    -------
    'pandas.DataFrame'
        DataFrame with record_ids as indexes and var_ids as columns
    """
    record_rows = []
    for record in record_list.records:
        record_id = record.record_id
        record_row = pd.Series(from_record(record), name=record_id)
        record_rows.append(record_row)

    return pd.concat(record_rows, axis=1).T


def from_record(record):
    """
    Extract variable id and value from Record

    Parameters
    ----------
    record_list : 'proto.Record'
        proto Record message from RecordList records

    Returns
    -------
    'dict'
        Dictionary matching var_id to value for all VarValues in record
    """
    return {var.var_id: from_value(var.value) for var in record.variables}


def from_value(value):
    """
    Extract variable value from VarValue

    Parameters
    ----------
    value : 'proto.VarValue'
        proto VarValue message from Records variables

    Returns
    -------
    'float' | 'int' | 'string'
        variable value
    """
    value_type = value.WhichOneof('value')
    if value_type == 'real_value':
        return value.real_value
    elif value_type == 'integer_value':
        return value.integer_value
    elif value_type == 'string_value':
        return value.string_value
    else:
        raise ProtoError('Cannot parse Value type!')


def from_record_table(record_table):
    """
    Extract record data from RecordTable

    Parameters
    ----------
    record_table : 'proto.RecordTable'
        proto RecordTable message from RecordData style

    Returns
    -------
    'ndarray' or 'pandas.DataFrame'
        array or pd.DataFrame of data rec_ids x var_ids in shape
    """
    var_ids = record_table.var_ids
    rec_ids = record_table.rec_ids
    table_values = from_list(record_table)
    array = np.array(table_values).reshape(len(rec_ids), len(var_ids))

    return pd.DataFrame(array, columns=var_ids, index=rec_ids)


def from_list(record_table):
    """
    Extract record data from RecordTable list

    Parameters
    ----------
    record_table : 'proto.RecordTable'
        proto RecordTable message from RecordData style

    Returns
    -------
    'list'
        list of value in record_table: either floats, ints, or strings
    """
    list_type = record_table.WhichOneof('list')
    if list_type == 'reals':
        return record_table.reals.values
    elif list_type == 'integers':
        return record_table.integers.values
    elif list_type == 'strings':
        return record_table.strings.values
    else:
        raise ProtoError('Cannot parse RecordTable list!')


def empty_data_chunk(records_data):
    """
    Check to see if data chunk is empty

    Parameters
    ----------
    records_data : 'proto.data'
        proto data message from Response type

    Returns
    -------
    empty : 'bool'
        Returns True if data chunk is empty, else False
    """
    if records_data.WhichOneof('style') == 'list':
        data = records_data.list.records

        if len(data) == 0:
            empty = True
        else:
            empty = False
    else:
        empty = False

    return empty


def handle_data_response(response):
    """
    Extracts records_data from each server response message

    Parameters
    ----------
    response : 'list'
        list of proto Response messages

    Returns
    -------
    'pandas.DataFrame'
        Concatenated data from each response message
    """
    records_data = []
    for message in response:
        data = message.data
        if not empty_data_chunk(data):
            records_data.append(from_record_data(data))

    return pd.concat(records_data)


def request_work(model_id, inputs, request_id, version=4):
    """
    Create request for records data

    Parameters
    ----------
    model_id : 'string'
        Id of model for which to requst records_data
    inputs : 'dict'
        Dictionary of {var_id: value} pairs
    request_id : 'int'
        Unique request id
    version : 'int'
        Google protobuf version, default = 4

    Returns
    -------
    request : 'proto.Request'
        proto Request message for records_data
    """
    request = proto.Request()
    request.version = version
    request.id.value = request_id
    work_request = request.work
    work_request.model_id = model_id

    var_values = []
    for key, value in inputs.items():
        var_values.append(set_var_value(key, value))

    work_request.inputs.extend(var_values)

    return request


def set_var_value(var_id, value):
    """
    set proto.VarValue message

    Parameters
    ----------
    var_id : 'int'
        input var_id
    value : 'float'|'int'|'string'
        input value

    Returns
    -------
    var_value : 'proto.VarValue'
        Filled proto.VarValue message
    """
    var_value = proto.VarValue()
    var_value.var_id = var_id
    if isinstance(value, float):
        var_value.value.real_value = value
    elif isinstance(value, int):
        var_value.value.integer_value = value
    elif isinstance(value, str):
        var_value.value.string_value = value
    else:
        raise ProtoError('Cannot parse value type')

    return var_value
