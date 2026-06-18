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
