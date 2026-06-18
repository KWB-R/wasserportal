# Obtain a JWT Bearer Token from ThingsBoard (Username / Password Login)

Calls `POST /api/auth/login` with a username/password pair and returns
the JWT access token. This is the **standard ThingsBoard REST API
authentication** and the only one available on self-hosted ThingsBoard
Community Edition: unlike the account-level *API key* (a ThingsBoard
Cloud convenience, generated under *Account \> Security \> API keys*),
every edition – CE, PE and Cloud – accepts a username/password login.

## Usage

``` r
tb_login(
  username = Sys.getenv("TB_USERNAME"),
  password = Sys.getenv("TB_PASSWORD"),
  host = tb_default_host()
)
```

## Arguments

- username:

  ThingsBoard user (usually the account e-mail). Defaults to env var
  `TB_USERNAME`.

- password:

  ThingsBoard password. Defaults to env var `TB_PASSWORD`.

- host:

  base URL of the ThingsBoard instance, without trailing slash. Defaults
  to env var `TB_HOST` if set, otherwise `"https://thingsboard.cloud"`.
  Use e.g. `"https://dashboards.inowas.org"` for a self-hosted instance.

## Value

the JWT access token as a character scalar, ready to be sent in an
`X-Authorization: Bearer <token>` request header.

## Details

The token is short-lived (ThingsBoard's default JWT expiration is 2.5
h), which is ample for the one-off device setup done by
[`tb_setup_devices`](https://kwb-r.github.io/wasserportal/reference/tb_setup_devices.md):
the subsequent telemetry push uses the per-device access token, not this
JWT, so no token refresh is implemented.

Transient failures (HTTP 408 / 429 / 500 / 502 / 503 / 504 and transport
dropouts) are retried with exponential backoff, matching the predicate
used for the telemetry POSTs so a flaky upstream does not abort the
device-setup run on the first 5xx.

## Credentials in error output

On a non-2xx response this helper surfaces an excerpt of the server's
response body (via `tb_error_body()`, up to ~800 chars) in the R error
message, and
[`httr2::req_retry()`](https://httr2.r-lib.org/reference/req_retry.html)
prints retry messages to stderr. Stock ThingsBoard only echoes back the
error description, not the request payload, so the password does not
leak. If a self-hosted instance or reverse proxy is configured to echo
request fields back in the error body, that excerpt would surface in R
errors and – when captured with `2>&1` – in CI logs. Mask the relevant
secrets in such environments.

## Examples

``` r
if (FALSE) { # \dontrun{
Sys.setenv(
  TB_HOST = "https://dashboards.inowas.org",
  TB_USERNAME = "me@example.org",
  TB_PASSWORD = "secret"
)
token <- tb_login()
} # }
```
