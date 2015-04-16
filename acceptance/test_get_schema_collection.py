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
        data = {
            '$schema': 'http://json-schema.org/draft-03/hyper-schema#',
            'collectionName': urls.TEST_COLLECTION_NAME
        }
        requests.post(
            urls.ITEM_SCHEMAS_URL,
            data=json.dumps(data),
            headers={
                'content-type': 'application/json'
            })

        cls.response = requests.get(
            urls.ITEM_SCHEMAS_URL +
            '?filter[perPage]=1000&filter[order]=collectionName')

        cls.json_response = cls.response.json()

    def test_status_code(self):
        self.response.status_code.should.equal(200)

    def test_include_items(self):
        names = [item['collectionName'] for item in self.json_response['items']]
        names.should.contain(urls.TEST_COLLECTION_NAME)
        names.should.have.length_of(1)

    def test_include_schema(self):
        self.json_response['items'][0].should.have.key('$schema')

class TestWithWhereFilter(TestCase):
    @classmethod
    def setUpClass(cls):
        cls.response = requests.get(
            urls.TEST_COLLECTION_URL +
            '?filter[where][name]=test-jackie-tekilla')

        cls.json_response = cls.response.json()

    def test_include_items(self):
        names = set([item['name'] for item in self.json_response['items']])
        names.should.have.length_of(0)
