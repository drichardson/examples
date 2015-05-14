package main

import (
	"crypto/x509"
	"encoding/pem"
	"fmt"
)

var caCertPEM string = `-----BEGIN CERTIFICATE-----
MIIDIDCCAgigAwIBAgIJAJyrSCEGC+PEMA0GCSqGSIb3DQEBCwUAMBQxEjAQBgNV
BAMTCVN0aXRjaCBDQTAeFw0xNTA1MTMwMTM4NDNaFw0yNTA1MTAwMTM4NDNaMBQx
EjAQBgNVBAMTCVN0aXRjaCBDQTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoC
ggEBALH2+JCLOI3W8L9OAMQ5Od9JOIMzrBzg/IBAU6HZaP+r8VH/sdWEk5LzZgSm
mDg55L5WZsC8fWo03AtE9c5XTZ6lzjw0CeAg72aeE5Nd054CzX/XyV5NfOZgGOxp
m6Ng2JoZeUvEgvuk4AbCbEyn+k/IwJ71uWqWiVY4vDw9WHPkW29juy3aib4feWtI
6/pGy3WY8mZ9MV1Efvmkz0LxwjEek0irzhS/aZ08gKE6IqN7/JStm9+2Q9nzOhDb
/SnyuO1ZWpVa27LeVeVW3vuJ1j49fc6IIL/09bvaYT60FZH5U4oefOSMqTZqTlWi
hSoYGDgvQEB15ovzInU9tO9h3C0CAwEAAaN1MHMwHQYDVR0OBBYEFPvbQ7XVRG11
dD1bBKHQhmTZ0ADMMEQGA1UdIwQ9MDuAFPvbQ7XVRG11dD1bBKHQhmTZ0ADMoRik
FjAUMRIwEAYDVQQDEwlTdGl0Y2ggQ0GCCQCcq0ghBgvjxDAMBgNVHRMEBTADAQH/
MA0GCSqGSIb3DQEBCwUAA4IBAQBACYyfT3RK5dip4kINF+0ONd902MJRqjx6I1e3
LcLMFr3AlD/XQr9QdmojLfW4rjK5K9DW9O0Jpph3GkmZqGkSv4eovcSB6VcgQ0in
FT11AttOdn9xOFk7Y8UVfzS+twA5Cu2+IpI/16Dwz/dim3uwLQEHMlRQFkGv6UA7
73J8OG9HF1OLHHkGhII2s5me0O3iyCkSsRogml5N0KJWTERFx9bHfIOQ3D0/uPhq
AaleV7dSI0gW+XcvzrLnR5y25oGlJzZ9Wk6XILz0M2qA0wwVNrED/WGZkbg6rjwO
wk1nmqWPAcdECxiRHRul9Ftxe09zvNKreJQJiuiJBhQMn0UG
-----END CERTIFICATE-----`

var serverCertPEM_localhost = `-----BEGIN CERTIFICATE-----
MIICoTCCAYmgAwIBAgIBAjANBgkqhkiG9w0BAQsFADAUMRIwEAYDVQQDEwlTdGl0
Y2ggQ0EwHhcNMTUwNTEzMjMwMzA2WhcNMjAwNTExMjMwMzA2WjAUMRIwEAYDVQQD
Ewlsb2NhbGhvc3QwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDIKg3A
xHfX44QWBojAbJTS8qHBcoljLyv7zSQOu2Gs5H5Dgg+sEkAFeePWtnRMP6wkCl/y
W9GQh54Rx2slCNU6PIK34qBFd9jXh1/OSzjHieS9/PD2O9ExiwcKG1hItTEUm6OM
Ceer5/Z1/DL8cpvkmVrYGpTrHi8ZkSdJ+pBOhsunu6mVfEt8oaZ78kEbJ6+1PZs9
F2vvMCZYsonrndCTnnSlNg1YFCAlA1UprFGnCZBbuwTmvp+lhJMOXXKlle6pOgA5
S7hv8LxCQcp+ruud0CeQsA49p1CPH/Ez4lcXbppVH77M5fgAfzgvDjxxPRpE0p94
BgdehKR8hTxXdtAHAgMBAAEwDQYJKoZIhvcNAQELBQADggEBACaCiU/joUiCzrzR
yFL3vYNpg/dVVucysRpYxX8s38+4u2HVZU4CO0ghx47p4dRBxJ5WDbq2hCqfCp9N
WSjIIaCIgH9EH5igrafz/SVfp/1vOBKNw7I0A7y48WvRa9vCmRE5HmiyNrvttK3v
K5i0GF4/3YBNT1lWIwsQ8JAF3Y8A+XzXDetuL5a8F53pbgj9TzYuFr7HwTnkSLim
THZ5BZFiteeLL2IcHJHtb1PhdKlnC1+gfom6nENshRWf/+faYaBP9sr/aARTGfSF
6s9X1Tvidl7Jxo7ZPqSyPwoJM67zokLZAAv+3c28BCb3ARrvrPYQrUjmkBtsV+su
+5UwuqY=
-----END CERTIFICATE-----`

func main() {
	pemBlock, _ := pem.Decode([]byte(caCertPEM))
	assertneq(pemBlock, nil, "Couldn't decode certificate in PEM format.")
	asserteq(pemBlock.Type, "CERTIFICATE", "Not a certificate")
	caCert, err := x509.ParseCertificate(pemBlock.Bytes)
	asserteq(err, nil, "Error parsing certificate.", err)
	assertneq(caCert, nil, "CA Certificate is nil")
	asserteq(caCert.IsCA, true, "Certificate is not a Certificate Authority")

	pool := x509.NewCertPool()
	//var pool *x509.CertPool
	//ok := pool.AppendCertsFromPEM([]byte(caCertPEM))
	//asserteq(ok, true, "Failed to add PEM certificate to pool.")
	pool.AddCert(caCert)

	pemBlock, _ = pem.Decode([]byte(serverCertPEM_localhost))
	assertneq(pemBlock, nil)
	asserteq(pemBlock.Type, "CERTIFICATE")
	serverCert, err := x509.ParseCertificate(pemBlock.Bytes)
	asserteq(err, nil, "Error parsing certificate.", err)
	assertneq(serverCert, nil, "CA Certificate is nil")
	asserteq(serverCert.IsCA, false, "Certificate is a Certificate Authority but shouldn't be")
	opts := x509.VerifyOptions{}
	opts.DNSName = "localhost"
	opts.Roots = pool
	_, err = serverCert.Verify(opts)
	asserteq(err, nil, "Error verifying certificate.", err)
	err = serverCert.VerifyHostname("localhost")
	asserteq(err, nil, "Error verifying hostname.", err)

	fmt.Println("OK")
}

func asserteq(v1 interface{}, v2 interface{}, args ...interface{}) {
	if v1 != v2 {
		panic(fmt.Sprintf("Assertion Failed: %v != %v. ", v1, v2) + fmt.Sprint(args...))
	}
}

func assertneq(v1 interface{}, v2 interface{}, args ...interface{}) {
	if v1 == v2 {
		panic(fmt.Sprintf("Assertion Failed: %v == %v. ", v1, v2) + fmt.Sprint(args...))
	}
}
