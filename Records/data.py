"""
Package for data requests

Created by: Michael Rossol Feb. 2017
"""
import numpy as np
import pandas as pd
import records_def_4_pb2 as proto


__all__ = ['request_records_data', 'from_record_data',
           'from_record_list', 'from_record', 'from_value',
           'from_record_table', 'from_list']


def request_records_data(model_id, request_id, max_records=0,
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
    'ndarray' or 'pd.DataFrame'
        Return records_data as either and ndarray or pd.DataFrame
    """
    style = record_data.WhichOneof('style')
    if style == 'list':
        return from_record_list(record_data.list)
    elif style == 'table':
        return from_record_table(record_data.table)
    else:
        raise Exception('Cannot parce RecordData style')


def from_record_list(record_list):
    """
    Extract record data from RecordList
    Parameters
    ----------
    record_list : 'proto.RecordList'
        proto RecordList message from RecordData style

    Returns
    -------
    'pd.DataFrame'
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
    'float', 'int', or 'string'
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
        raise Exception('Cannot parse Value type')


def from_record_table(record_table, labels=False):
    """
    Extract record data from RecordTable
    Parameters
    ----------
    record_table : 'proto.RecordTable'
        proto RecordTable message from RecordData style
    labels : 'bool'
        Option to return data as a pd.DataFrame with
        indexes = rec_ids and columns = var_ids

    Returns
    -------
    'ndarray' or 'pd.DataFrame'
        array or pd.DataFrame of data rec_ids x var_ids in shape
    """
    var_ids = record_table.var_ids
    rec_ids = record_table.rec_ids
    table_values = from_list(record_table)
    array = np.array(table_values).reshape(len(rec_ids), len(var_ids))

    if labels:
        return pd.DataFrame(array, columns=var_ids, index=rec_ids)
    else:
        return array


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
        raise Exception('Cannot parse RecordTable list')


def empty_data_chunk(record_data):
    """
    Check to see if data chunk is empty
    Parameters
    ----------
    records_data : 'proto.data'
        proto data message from Response type

    Returns
    -------
    'bool'
        Returns True if data chunk is empty, else False
    """
    try:
        data = record_data.list.records
        if len(data) == 0:
            return True
        else:
            return False
    except Exception:
        return False
