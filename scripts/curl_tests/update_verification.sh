#!/bin/sh
curl -v --data "{\"object\":\"user\",\"entry\":[{\"uid\":\"1335845740\",\"changed_fields\":[\"name\",\"picture\"],\"time\":232323},{\"uid\":\"1234\",\"changed_fields\":[\"friends\"],\"time\":232325}]}" -H "x-hub-signature:sha1=534985d2be5f2df69cae7cc5e23be204add4f499" "http://localhost:5498/any_app"
