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

        cls.response = requests.get(urls.TEST_ITEM_SCHEMA_URL)
        cls.json_response = cls.response.json()

    def test_status_code(self):
        self.response.status_code.should.equal(200)

    def test_collection_name_value(self):
        self.json_response['collectionName'].should.equal(urls.TEST_COLLECTION_NAME)

    def test_include_schema(self):
        self.json_response.should.have.key('$schema')

class TestNotFoundGET(TestCase):
    @classmethod
    def setUpClass(cls):
        cls.response = requests.get(urls.item_schema_url('not-found'))

    def test_status_code(self):
        self.response.status_code.should.equal(404)
