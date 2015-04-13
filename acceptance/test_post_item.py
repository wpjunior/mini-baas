# -*- coding: utf-8 -*-

import requests
import json
import sure
import re
UUID4_HEX = re.compile('[0-9a-f]{12}4[0-9a-f]{3}[89ab][0-9a-f]{15}\Z', re.I)

from unittest import TestCase, skip

TARGET_URL = 'http://localhost:8080'

def url(url):
    return ''.join((TARGET_URL, url))

TEST_COLLECTION_URL = url('/api/test-people')

class TestSuccessfullyPost(TestCase):
    @classmethod
    def setUpClass(cls):
        data = {'name': 'Alice'}
        cls.response = requests.post(
            TEST_COLLECTION_URL,
            data=json.dumps(data),
            headers={
                'content-type': 'application/json'
            })

        cls.json_response = cls.response.json()

    def test_status_code(self):
        self.response.status_code.should.equal(201)

    @skip("future feature")
    def test_schema_link_header(self):
        schema_url = url('/api/item-schema/people')
        self.response.headers['link'].should.equal(schema_url)
        self.response.headers['content_type'].should.equal(
            'application/json; charset=utf-8; profile="%s"' % schema_url)

    @skip("future feature")
    def test_location_header(self):
        location_url = url('/api/people/%s' % self.json_response['id'])
        self.response.headers['location'].should.equal(location_url)

    @skip("future feature")
    def test_created_field(self):
        self.json_response.should.have.key('created').not_be.being.none

    @skip("future feature")
    def test_modified_field(self):
        self.json_response.should.have.key('modified').not_be.being.none

    def test_primary_key(self):
        self.json_response.should.have.key('id')
        self.json_response['id'].should.match(
            r'^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$')

    def test_posted_value(self):
        self.json_response['name'].should.equal('Alice')

class TestPostInvalidBody(TestCase):
    @classmethod
    def setUpClass(cls):
        data = '..A..B..'
        cls.response = requests.post(
            TEST_COLLECTION_URL,
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
            TEST_COLLECTION_URL,
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