#!/bin/sh
curl -v --data "{\"object\":\"user\",\"entry\":[{\"uid\":1335845740,\"changed_fields\":[\"name\",\"picture\"],\"time\":232323},{\"uid\":1234,\"changed_fields\":[\"friends\"],\"time\":232325}]}" -H "Content-Type:application/json" "http://localhost:5498/"
