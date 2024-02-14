SET client_min_messages TO WARNING;
CREATE EXTENSION IF NOT EXISTS pgtap;
RESET client_min_messages;

BEGIN;

SET search_path = mock,pg_catalog,public;

SELECT plan(6);

SET client_min_messages TO WARNING;
TRUNCATE TABLE users, user_columns, woulds, user_woulds CASCADE;
RESET client_min_messages;

-- these don't particularly have to match the real defaults, we just want to
-- test the defaulting mechanism
INSERT INTO woulds (name, is_default)
VALUES ('Hang out sometime', true), ('Go on a date or something', true);

INSERT INTO users (facebook_id) VALUES ('thisUser');
SELECT set_config('jwt.claims.facebookUserId', 'thisUser', true);
SELECT bag_eq(
  $$ SELECT current_user_id() $$,
  $$ SELECT user_id FROM users $$,
  'current_user_id() returns id of user'
);

INSERT INTO users (facebook_id) VALUES ('thatUser');

INSERT INTO woulds (name) VALUES ('secret third thing');

SELECT bag_eq(
  $$ SELECT added_by_id FROM woulds WHERE name = 'secret third thing' $$,
  $$ SELECT current_user_id() $$,
  'added_by_id filled in by trigger'
);

SELECT throws_ok(
  $$
    INSERT INTO woulds (name)
    SELECT 'secret third thing'
  $$,
  '23505', 'duplicate key value violates unique constraint "woulds_name_key"',
  'no duplicate names'
);

SELECT is(
  get_my_columns(),
  (SELECT array_agg(would_id) FROM woulds WHERE is_default),
  'default columns'
);

INSERT INTO woulds (name) VALUES
  ('secret fourth thing'),
  ('secret fifth thing');

SELECT throws_ok(
  $$
    INSERT INTO woulds (name) VALUES ('secret sixth thing');
  $$,
  'P0001', 'Cannot create more than 3 columns every 3 days',
  'limit custom columns'
);

UPDATE mock.mock_now SET t = '2000-01-04 00:00:00+00';

SELECT lives_ok(
  $$
    INSERT INTO woulds (name, added_by_id)
    SELECT 'secret sixth thing', current_user_id()
  $$,
  'can insert another would after three days'
);

SELECT finish();

ROLLBACK;
