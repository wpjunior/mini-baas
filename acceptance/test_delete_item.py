# -*- coding: utf-8 -*-

import requests
import sure
import urls
import json

from unittest import TestCase, skip


class TestSuccessfullyDELETE(TestCase):
    @classmethod
    def setUpClass(cls):
        data = {'name': 'GET Alice'}
        post_json_response = requests.post(
            urls.TEST_COLLECTION_URL,
            data=json.dumps(data),
            headers={
                'content-type': 'application/json'
            }).json()

        pk = post_json_response['id']
        cls.response = requests.delete(urls.test_resource_url(pk))
        cls.get_response = requests.get(urls.test_resource_url(pk))

    def test_status_code(self):
        self.response.status_code.should.equal(204)

    def test_previous_url_status(self):
        self.get_response.status_code.should.equal(404)

class TestNotFoundDELETE(TestCase):
    @classmethod
    def setUpClass(cls):
        cls.response = requests.delete(urls.test_resource_url('not-found'))

    def test_status_code(self):
        self.response.status_code.should.equal(404)
