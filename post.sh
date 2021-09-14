#! /bin/bash
stamp=`date -Iseconds`
curl -X POST http://localhost:8000/ -H "Content-Type: text/turtle" --data-raw "@prefix : <http://localhost:8000/> . @prefix xsd: <http://www.w3.org/2001/XMLSchema#> . <> :hasTimestamp \"${stamp}\"^^xsd:dateTimeStamp ; :value \"${1}\"^^xsd:integer ."