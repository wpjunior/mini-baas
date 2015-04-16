# -*- coding: utf-8 -*-

import requests
import sure
import urls
import json

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

        url = urls.item_schema_url(urls.TEST_COLLECTION_NAME)

        cls.response = requests.delete(url)
        cls.get_response = requests.get(url)

    def test_status_code(self):
        self.response.status_code.should.equal(204)

    def test_previous_url_status(self):
        self.get_response.status_code.should.equal(404)

class TestNotFoundDELETE(TestCase):
    @classmethod
    def setUpClass(cls):
        cls.response = requests.delete(urls.item_schema_url('not-found'))

    def test_status_code(self):
        self.response.status_code.should.equal(404)
