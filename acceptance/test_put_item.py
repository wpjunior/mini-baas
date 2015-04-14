# -*- coding: utf-8 -*-

import requests
import json
import sure
import urls

from unittest import TestCase, skip


class TestSuccessfullyPUT(TestCase):
    @classmethod
    def setUpClass(cls):
        data = {'name': 'PUT Alice', }
        put_data = {'age': 30}

        cls.before_item = requests.post(
            urls.TEST_COLLECTION_URL,
            data=json.dumps(data),
            headers={
                'content-type': 'application/json'
            }).json()

        cls.response = requests.put(
            urls.test_resource_url(cls.before_item['id']),
            data=json.dumps(put_data),
            headers={
                'content-type': 'application/json'
            })
        cls.json_response = cls.response.json()

    def test_status_code(self):
        self.response.status_code.should.equal(200)

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

    @skip("future feature")
    def test_created_field(self):
        self.json_response.should.have.key('created').not_be.being.none

    @skip("future feature")
    def test_created_not_changed(self):
        self.json_response['created'].should.equal(self.before_item['created'])

    @skip("future feature")
    def test_modified_field(self):
        self.json_response.should.have.key('modified').not_be.being.none

    @skip("future feature")
    def test_modified_not_changed(self):
        self.json_response['modified'].should.not_be.equal(
            self.before_item['modified'])

    def test_primary_key(self):
        self.json_response.should.have.key('id')
        self.json_response['id'].should.match(
            r'^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$')

    def test_changed_value(self):
        self.json_response['name'].should.equal('PUT Alice')
        self.json_response['age'].should.equal(30)


class TestInvalidBodyPUT(TestCase):
    @classmethod
    def setUpClass(cls):
        data = {'name': 'PUT Alice'}
        put_data = '///invalid///'

        item = requests.post(
            urls.TEST_COLLECTION_URL,
            data=json.dumps(data),
            headers={
                'content-type': 'application/json'
            }).json()

        cls.response = requests.put(
            urls.test_resource_url(item['id']),
            data=put_data,
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
        self.json_response.should.equal({
            'id': 'my-custom-body', 'name': 'wilson'
        })
