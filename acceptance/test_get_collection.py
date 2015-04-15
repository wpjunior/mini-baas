# -*- coding: utf-8 -*-

import requests
import json
import sure
import re
import urls

from unittest import TestCase, skip


class TestSuccessfully(TestCase):
    @classmethod
    def setUpClass(cls):
        data = {'name': 'Alice (Get Collection)'}
        requests.post(
            urls.TEST_COLLECTION_URL,
            data=json.dumps(data),
            headers={
                'content-type': 'application/json'
            })

        data = {'name': 'Bob (Get Collection)'}
        requests.post(
            urls.TEST_COLLECTION_URL,
            data=json.dumps(data),
            headers={
                'content-type': 'application/json'
            })

        cls.response = requests.get(
            urls.TEST_COLLECTION_URL +
            '?filter[perPage]=1000&filter[order]=name')

        cls.json_response = cls.response.json()

    def test_status_code(self):
        self.response.status_code.should.equal(200)

    @skip("future feature")
    def test_schema_link_header(self):
        schema_url = urls.TEST_COLLECTION_SCHEMA_URL
        self.response.headers['link'].should.equal(schema_url)
        self.response.headers['content_type'].should.equal(
            'application/json; charset=utf-8; profile="%s"' % schema_url)

    def test_include_items(self):
        names = [item['name'] for item in self.json_response['items']]

        names.should.contain('Alice (Get Collection)')
        names.should.contain('Bob (Get Collection)')


class TestWithWhereFilter(TestCase):
    @classmethod
    def setUpClass(cls):
        data = {'name': 'jackie-tekilla'}
        requests.post(
            urls.TEST_COLLECTION_URL,
            data=json.dumps(data),
            headers={
                'content-type': 'application/json'
            })

        data = {'name': 'jackie-hunter'}
        requests.post(
            urls.TEST_COLLECTION_URL,
            data=json.dumps(data),
            headers={
                'content-type': 'application/json'
            })

        cls.response = requests.get(
            urls.TEST_COLLECTION_URL +
            '?filter[where][name]=jackie-tekilla')

        cls.json_response = cls.response.json()

    def test_include_items(self):
        names = set([item['name'] for item in self.json_response['items']])
        names.should.have.length_of(1)
