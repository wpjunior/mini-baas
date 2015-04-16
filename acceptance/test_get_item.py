# -*- coding: utf-8 -*-

import requests
import json
import sure
import re
import urls
UUID4_HEX = re.compile('[0-9a-f]{12}4[0-9a-f]{3}[89ab][0-9a-f]{15}\Z', re.I)

from unittest import TestCase, skip


class TestSuccessfully(TestCase):
    @classmethod
    def setUpClass(cls):
        data = {'name': 'GET Alice'}
        item = requests.post(
            urls.TEST_COLLECTION_URL,
            data=json.dumps(data),
            headers={
                'content-type': 'application/json'
            }).json()

        cls.response = requests.get(urls.test_resource_url(item['id']))
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
        self.json_response['name'].should.equal('GET Alice')


class TestNotFoundGET(TestCase):
    @classmethod
    def setUpClass(cls):
        cls.response = requests.get(urls.test_resource_url('not-found'))

    def test_status_code(self):
        self.response.status_code.should.equal(404)
