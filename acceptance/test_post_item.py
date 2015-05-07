# -*- coding: utf-8 -*-

import requests
import json
import sure
import re
import urls
UUID4_HEX = r'^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$'

from unittest import TestCase, skip


class TestSuccessfullyPOST(TestCase):
    @classmethod
    def setUpClass(cls):
        requests.delete(urls.TEST_ITEM_SCHEMA_URL)
        schema = {
            '$schema': 'http://json-schema.org/draft-03/schema#',
            'collectionName': urls.TEST_COLLECTION_NAME,
            'properties': {
                'name': {'type': 'string'}
            },
            'type': 'object'
        }

        requests.post(
            urls.collection_url('item-schemas'),
            data=json.dumps(schema),
            headers={
                'content-type': 'application/json'
            })
        data = {'name': 'POST Alice'}
        cls.response = requests.post(
            urls.TEST_COLLECTION_URL,
            data=json.dumps(data),
            headers={
                'content-type': 'application/json'
            })

        cls.json_response = cls.response.json()

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
        location_url = urls.test_resource_url(self.json_response['id'])
        self.response.headers['location'].should.equal(location_url)

    def test_created_field(self):
        self.json_response.should.have.key('created').not_be.being.none

    def test_modified_field(self):
        self.json_response.should.have.key('modified').not_be.being.none

    def test_primary_key(self):
        self.json_response.should.have.key('id')
        self.json_response['id'].should.match(
            r'^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$')

    def test_posted_value(self):
        self.json_response['name'].should.equal('POST Alice')

    def test_invalid_data(self):
        data = {'name': 1992}
        response = requests.post(
            urls.TEST_COLLECTION_URL,
            data=json.dumps(data),
            headers={
                'content-type': 'application/json'
            })

        response.status_code.should.equal(422)
        errors = response.json()
        errors.should.equal({'errors': {'/name': 'Wrong type'}})

class TestPostInvalidBody(TestCase):
    @classmethod
    def setUpClass(cls):
        data = '..A..B..'
        cls.response = requests.post(
            urls.TEST_COLLECTION_URL,
            data=data,
            headers={
                'content-type': 'application/json'
            })

    def test_status_code(self):
        self.response.status_code.should.equal(422)


class TestPostEmbeddedBody(TestCase):
    @classmethod
    def setUpClass(cls):
        data = {
            'name': 'wilson',
            'address': {
                'state': 'RJ', 'City': 'Rio de Janeiro'
        }}

        cls.response = requests.post(
            urls.TEST_COLLECTION_URL,
            data=json.dumps(data),
            headers={
                'content-type': 'application/json'
            })

        cls.json_response = cls.response.json()

    def test_status_code(self):
        self.response.status_code.should.equal(201)

    def test_embbedded_value(self):
        self.json_response['address'].should.equal({
            'state': 'RJ', 'City': 'Rio de Janeiro'
        })

class TestPOSTWithCustomId(TestCase):
    @classmethod
    def setUpClass(cls):
        data = {'id': 'my-custom-body', 'name': 'wilson'}

        cls.response = requests.post(
            urls.TEST_COLLECTION_URL,
            data=json.dumps(data),
            headers={
                'content-type': 'application/json'
            })

        cls.json_response = cls.response.json()

    def test_status_code(self):
        self.response.status_code.should.equal(201)

    def test_json_response_value(self):
        self.json_response['id'].should.equal('my-custom-body')
        self.json_response['name'].should.equal('wilson')
        self.json_response['versionId'].should.match(UUID4_HEX)
