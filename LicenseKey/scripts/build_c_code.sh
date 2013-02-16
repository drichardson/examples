openssl enc -base64 -d <public.pem | od -X  |ruby make_c_code.ruby

