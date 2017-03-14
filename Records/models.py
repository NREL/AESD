"""
Package for models request

Created by: Michael Rossol Feb. 2017
"""
import records_def_4_pb2 as proto

__all__ = ['request_model_metadata', 'from_model', 'from_variable',
           'from_models_metadata', 'handle_models_response']


def request_model_metadata(model_id, request_id, version=4):
    """
    Create request for model models_metadata
    Parameters
    ----------
    model_id : 'string'
        Id of model for which to requst models_metadata
        if None requests all models
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
    model_request = request.models_metadata
    if model_id is not None:
        model_request.model_id.value = model_id
    else:
        model_request.model_id.value = ''
    return request


def from_model(model):
    """
    Unpacks proto ModelsMeta message to dictionary
    Parameters
    ----------
    model : 'proto.ModelsMeta'
        proto ModelsMeta message from ModelsMetaList
    Returns
    -------
    'dict'
        Dictionary containing the model's metadata
    """
    return {'id': model.model_id,
            'name': model.model_name,
            'uri': model.model_uri,
            'variables': list(map(from_variable, model.variables))}


def from_variable(variable):
    """
    Unpacks proto VarMeta message to dictionary
    Parameters
    ----------
    model : 'proto.VarMeta'
        proto VarMeta message from list of variables in ModelsMeta
    Returns
    -------
    'dict'
        Dictionary containing the variable's metadata
    """
    return {'id': variable.var_id,
            'name': variable.var_name,
            'units': variable.units,
            'si': variable.si,
            'scale': variable.scale,
            'type': variable.type}


def from_models_metadata(models):
    """
    Extract metadata for each model in ModelsMetaList
    Parameters
    ----------
    models : 'proto.ModelsMetaList'
        proto ModelsMetaList message from Response type
    Returns
    -------
    'list'
        List of model's metadata dictionaries for each model in models
    """
    return list(map(from_model, models.models))


def handle_models_response(response):
    """
    Extract model metadata from each server response message
    Parameters
    ----------
    response : 'list'
        list of proto Response messages
    Returns
    -------
    model_metadata : 'list'
        List of model's metadata dictionaries for each model in models
    """
    models_metadata = []
    for message in response:
        models_metadata.extend(from_models_metadata(message.models))

    return models_metadata
