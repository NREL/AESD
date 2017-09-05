import click
import os
import pickle
from .records import Records


@click.group()
def cli():
    pass


@cli.command()
@click.option('--server_url', help='url for server')
@click.option('--model_id', default=None,
              help='Id for model whose meta data is being requested \
(None returns all models on server)')
@click.option('--timeout', default=60,
              help='timeout in sec for communication with server', type=int)
@click.option('--file_out', default=None,
              help='File path to save results')
def request_model_metadata(server_url, model_id, timeout, file_out):
    with Records(server_url, timeout=timeout) as request:
        models = request.get_model_info(model_id)

        if file_out is not None:
            if not file_out.endswith('.pkl'):
                file_out = os.path.splitext(file_out)[0] + '.pkl'
            pickle.dump(models, open(file_out, 'rb'))

        print(models)


@cli.command()
@click.option('--server_url', help='url for server')
@click.option('--model_id', help='Id of model whose data is being requested')
@click.option('--max_records', default=1000,
              help='Number or records being request \
(0 will return all records)', type=int)
@click.option('--variable_ids', default=None,
              help='List of variable ids (ints) to be requested \
(None will return all variables)', type=list)
@click.option('--bookmark_id', default=None,
              help='Request data corresponding to bookmark id')
@click.option('--timeout', default=60,
              help='timeout in sec for communication with server', type=int)
@click.option('--file_out', default=None,
              help='File path to save results')
def request_records_data(server_url, model_id, max_records, variable_ids,
                         bookmark_id, timeout, file_out):
    with Records(server_url, timeout=timeout) as request:
        data = request.get_data(model_id, max_records=max_records,
                                variable_ids=variable_ids,
                                bookmark_id=bookmark_id)

        if file_out is not None:
            if not file_out.endswith('.pkl'):
                file_out = os.path.splitext(file_out)[0] + '.pkl'
            pickle.dump(data, open(file_out, 'rb'))

        print(data)


@cli.command()
@click.option('--server_url', help='url for server')
@click.option('--model_id',
              help='Id of model whose bookmarks are being requested')
@click.option('--bookmark_id', default=None,
              help='Request data corresponding to bookmark id \
(None will return all bookmarks)')
@click.option('--timeout', default=60,
              help='timeout in sec for communication with server', type=int)
@click.option('--file_out', default=None,
              help='File path to save results')
def request_bookmark_meta(server_url, model_id, bookmark_id, timeout,
                          file_out):
    with Records(server_url, timeout=timeout) as request:
        bookmarks = request.get_bookmark_info(model_id, bookmark_id)

        if file_out is not None:
            if not file_out.endswith('.pkl'):
                file_out = os.path.splitext(file_out)[0] + '.pkl'
            pickle.dump(bookmarks, open(file_out, 'rb'))

        print(bookmarks)


@cli.command()
@click.option('--server_url', help='url for server')
@click.option('--model_id',
              help='Id of model where new bookmark is being saved')
@click.option('--name', help='Name of new bookmark')
@click.option('--content', help='Contents of bookmark: list is a bookmark set, \
tuple is a bookmark interval', type=(list, tuple))
@click.option('--timeout', default=60,
              help='timeout in sec for communication with server', type=int)
@click.option('--file_out', default=None,
              help='File path to save results')
def save_bookmark(server_url, model_id, name, content, timeout,
                  file_out):
    with Records(server_url, timeout=timeout) as request:
        new_bookmark = request.save_bookmark(model_id, name, content)

        if file_out is not None:
            if not file_out.endswith('.pkl'):
                file_out = os.path.splitext(file_out)[0] + '.pkl'
            pickle.dump(new_bookmark, open(file_out, 'rb'))

        print(new_bookmark)


@cli.command()
@click.option('--server_url', help='url for server')
@click.option('--model_id',
              help='Id of model where new bookmark is being saved')
@click.option('--inputs', help='Dictionary of {var_id: value} pairs',
              type=dict)
@click.option('--timeout', default=60,
              help='timeout in sec for communication with server', type=int)
@click.option('--file_out', default=None,
              help='File path to save results')
def request_work(server_url, model_id, inputs, timeout, file_out):
    with Records(server_url, timeout=timeout) as request:
        work = request.do_work(model_id, inputs)

        if file_out is not None:
            if not file_out.endswith('.pkl'):
                file_out = os.path.splitext(file_out)[0] + '.pkl'
            pickle.dump(work, open(file_out, 'rb'))

        print(work)
