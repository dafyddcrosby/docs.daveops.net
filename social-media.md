# Social Media


# Facebook

- <https://developers.facebook.com>
- <https://developers.facebook.com/tools/debug>


## Graph API

```
# Get details of calling user
/me
```


# Twitter

- <https://dev.twitter.com>
- <https://developer.twitter.com/en/docs/api-reference-index>


## Rate Limiting

Up-to-date limits at <https://developer.twitter.com/en/docs/basics/rate-limits>

A window is 15 minutes

| Endpoint                                              | Resource family | Requests / window (user auth) | Requests / window (app auth) |
|----------------------------------------------------- |--------------- |----------------------------- |---------------------------- |
| GET account/verify<sub>credentials</sub>              | application     | 75                            | 0                            |
| GET application/rate<sub>limit</sub><sub>status</sub> | application     | 180                           | 180                          |
| GET favorites/list                                    | favorites       | 75                            | 75                           |
| GET followers/ids                                     | followers       | 15                            | 15                           |
| GET followers/list                                    | followers       | 15                            | 15                           |
| GET friends/ids                                       | friends         | 15                            | 15                           |
| GET friends/list                                      | friends         | 15                            | 15                           |
| GET friendships/show                                  | friendships     | 180                           | 15                           |
| GET geo/id/:place<sub>id</sub>                        | geo             | 75                            | 0                            |
| GET help/configuration                                | help            | 15                            | 15                           |
| GET help/languages                                    | help            | 15                            | 15                           |
| GET help/privacy                                      | help            | 15                            | 15                           |
| GET help/tos                                          | help            | 15                            | 15                           |
| GET lists/list                                        | lists           | 15                            | 15                           |
| GET lists/members                                     | lists           | 900                           | 75                           |
| GET lists/members/show                                | lists           | 15                            | 15                           |
| GET lists/memberships                                 | lists           | 75                            | 75                           |
| GET lists/ownerships                                  | lists           | 15                            | 15                           |
| GET lists/show                                        | lists           | 75                            | 75                           |
| GET lists/statuses                                    | lists           | 900                           | 900                          |
| GET lists/subscribers                                 | lists           | 180                           | 15                           |
| GET lists/subscribers/show                            | lists           | 15                            | 15                           |
| GET lists/subscriptions                               | lists           | 15                            | 15                           |
| GET search/tweets                                     | search          | 180                           | 450                          |
| GET statuses/lookup                                   | statuses        | 900                           | 300                          |
| GET statuses/mentions<sub>timeline</sub>              | statuses        | 75                            | 0                            |
| GET statuses/retweeters/ids                           | statuses        | 75                            | 300                          |
| GET statuses/retweets<sub>of</sub><sub>me</sub>       | statuses        | 75                            | 0                            |
| GET statuses/retweets/:id                             | statuses        | 75                            | 300                          |
| GET statuses/show/:id                                 | statuses        | 900                           | 900                          |
| GET statuses/user<sub>timeline</sub>                  | statuses        | 900                           | 1500                         |
| GET trends/available                                  | trends          | 75                            | 75                           |
| GET trends/closest                                    | trends          | 75                            | 75                           |
| GET trends/place                                      | trends          | 75                            | 75                           |
| GET users/lookup                                      | users           | 900                           | 300                          |
| GET users/search                                      | users           | 900                           | 0                            |
| GET users/show                                        | users           | 900                           | 900                          |
| GET users/suggestions                                 | users           | 15                            | 15                           |
| GET users/suggestions/:slug                           | users           | 15                            | 15                           |
| GET users/suggestions/:slug/members                   | users           | 15                            | 15                           |


## Cards

Validator <https://cards-dev.twitter.com/validator>

```html
<!-- Twitter Card data -->
<meta name="twitter:card" content="summary">
<meta name="twitter:site" content="@publisher_handle">
<meta name="twitter:title" content="Page Title">
<meta name="twitter:description" content="Page description less than 200 characters">
<meta name="twitter:creator" content="@author_handle">
<-- Twitter Summary card images must be at least 120x120px -->
<meta name="twitter:image" content="http://www.example.com/image.jpg">
```