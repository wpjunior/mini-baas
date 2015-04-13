TARGET_URL = 'http://localhost:8080'

def url(url):
    return ''.join((TARGET_URL, url))

def collection_url(collection_name):
    return url('/api/%s' % collection_name)

def resource_url(collection_name, pk):
    return "%s/%s" % (collection_url(collection_name), pk)

def item_schema_url(collection_name):
    return resource_url('item-schemas', collection_name)

TEST_COLLECTION_NAME = 'test-people'
TEST_COLLECTION_URL = collection_url(TEST_COLLECTION_NAME)
TEST_ITEM_SCHEMA_URL = item_schema_url(TEST_COLLECTION_NAME)

def test_resource_url(pk):
    return resource_url(TEST_COLLECTION_NAME, pk)
