# -*- coding: utf-8 -*-

import requests
import json
import sure
import urls

from unittest import TestCase, skip


class TestSuccessfully(TestCase):
    @classmethod
    def setUpClass(cls):
        data = {
            '$schema': 'http://json-schema.org/draft-03/hyper-schema#',
            'collectionName': urls.TEST_COLLECTION_NAME
        }
        cls.response = requests.post(
            urls.collection_url('item-schemas'),
            data=json.dumps(data),
            headers={
                'content-type': 'application/json'
            })

        cls.schema = cls.response.json()

    def test_status_code(self):
        self.response.status_code.should.equal(201)

    @skip("future feature")
    def test_schema_link_header(self):
        schema_url = urls.TEST_ITEM_SCHEMA_URL
        self.response.headers['link'].should.equal(schema_url)
        self.response.headers['content_type'].should.equal(
            'application/json; charset=utf-8; profile="%s"' % schema_url)

    @skip("future feature")
    def test_location_header(self):
        location_url = urls.item_schema_url(self.schema['collectionName'])
        self.response.headers['location'].should.equal(location_url)

    @skip("future feature")
    def test_created_field(self):
        self.schema['properties'].should.have.key('created').not_be.being.none

    @skip("future feature")
    def test_modified_field(self):
        self.schema['properties'].should.have.key('modified').not_be.being.none

    @skip("future feature")
    def test_version_id_field(self):
        self.schema['properties'].should.have.key('versionId').not_be.being.none

    def test_primary_key(self):
        self.schema['collectionName'].should.equal(urls.TEST_COLLECTION_NAME)

    def test_schema_key(self):
        self.schema['$schema'].should.equal(
            'http://json-schema.org/draft-03/hyper-schema#')


class TestPostInvalidBody(TestCase):
    @classmethod
    def setUpClass(cls):
        data = '..A..B..'
        cls.response = requests.post(
            urls.collection_url('item-schemas'),
            data=data,
            headers={
                'content-type': 'application/json'
            })

    def test_status_code(self):
        self.response.status_code.should.equal(422)
