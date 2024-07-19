BEGIN;
CREATE SCHEMA mock;
GRANT USAGE ON SCHEMA mock TO api;

SET search_path = mock,pg_catalog,public;

CREATE TABLE mock.mock_now (t timestamptz);
INSERT INTO mock.mock_now VALUES ('2000-01-01 00:00:00+00');
GRANT SELECT, INSERT, UPDATE ON mock.mock_now TO api;

CREATE FUNCTION mock.now() RETURNS timestamptz
  LANGUAGE sql SECURITY INVOKER STABLE PARALLEL SAFE
  BEGIN ATOMIC
    SELECT t FROM mock.mock_now;
  END;
COMMIT;
