This document describes the technical interview exercise for [ACI Learning](https://acilearning.com/).
We think you can complete the exercise in around four hours.
Please don't spend much longer than that on it.
Rather than having you spend a lot of time on it,
we would prefer to discuss an incomplete solution with you.

## Prompt

You should implement an HTTP server for managing courses.
The API specification is given below.
You can use any language, framework, and database that you would like.
Please include a README that describes the tech stack and how to run it.
Although ACI Learning uses Haskell, we are comfortable reviewing code in any language.

## Notes

- Persistence is optional.
  This means that your server needs to remember things while it is running,
  but you do not need to remember things across restarts.
  You are welcome to use a database like PostgreSQL or something in-memory like SQLite.

- All timestamps should use the ISO 8601 / RFC 3339 format.
  Specifically this format: `%Y-%m-%dT%H:%M:%SZ`.
  For example: `2001-02-03T04:05:06Z`.

- Read the specification carefully.
  We do not think there are any errors or inconsistencies.

## Data

This exercise only deals with a single data model: a course.
It should have the following fields:

- `id`: Required. The primary key, usually a positive integer.
- `name`: Required. A string. Must have at least one non-space character, and must be unique among non-deleted courses.
- `status`: Required. A string. Must be one of `"scheduled"`, `"in_production"`, or `"available"`. Note that these statuses are `snake_case` rather than `camelCase`.
- `createdAt`: Required. A timestamp that stores when the course was created.
- `updatedAt`: Required. A timestamp that stores when the course was most recently updated.
- `deletedAt`: Optional. A timestamp that stores when the course was deleted.

Note that this describes a course in the database,
not necessarily how it should look as JSON.

## Endpoints

All endpoints should return JSON with the `Content-Type` header set to `application/json`.

### GET /courses

- Returns a list of non-deleted courses, sorted by when they were created with the oldest ones first.
- Response body should look like this:
  ``` js
  {
    "courses": [
      {
        "id": 123,
        "name": "Linux for Children"
      }, ...
    ]
  }
  ```

### GET /courses/:id
- If the course does not exist, return an HTTP 404 Not Found.
- If the course is deleted, return an HTTP 410 Gone.
- Return an HTTP 200 OK.
- Response body should look like this:
  ``` js
  {
    "id": 123,
    "name": "Linux for Children",
    "status": "available",
    "created_at": "2001-02-03T04:05:06Z", // snake_cake
    "updatedAt": "2001-02-03T04:05:06Z" // camelCase
  }
  ```

### POST /courses

- Creates a new course.
- The request body should look like this:
  ``` js
  {
    "name": "Linux for Children",
    "status": "available"
  }
  ```
- If the input is invalid, return an HTTP 400 Bad Request.
- Sets the `createdAt` and `updatedAt` fields to the current time.
- Return an HTTP 201 Created.
  - There should be a `Location` header pointing to `/courses/:id`.

### PUT /courses/:id

- The input is the same as the `POST` endpoint.
  - Both the `"name"` and `"status"` fields are required (no partial updates).
- If the input is invalid, return an HTTP 400 Bad Request.
- If the course does not exist, return an HTTP 404 Not Found.
- If the course is deleted, return an HTTP 410 Gone.
- Sets the `updatedAt` field to the current time.

### DELETE /courses/:id

- If the course does not exist, return an HTTP 404 Not Found.
- If the course is deleted, return an HTTP 410 Gone.
- Sets the `deletedAt` and `updatedAt` fields to the current time.
- Return an HTTP 204 No Content.

## Bonus

The following items are worth a little extra but are by no means required:

- Returning good error messages when something goes wrong.
- Logging on the server about various things happening.
- Delivering a way to run your server with Docker or Docker Compose.

