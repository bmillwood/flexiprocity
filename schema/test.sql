SET client_min_messages TO WARNING;
CREATE EXTENSION IF NOT EXISTS pgtap;
RESET client_min_messages;

BEGIN;

SET search_path = mock,pg_catalog,public;

SELECT plan(7);

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

SELECT is(
  num_woulds_allowed(current_user_id()),
  1::bigint,
  'initially allowed one custom column'
);

INSERT INTO woulds (name, added_by_id)
SELECT 'secret third thing', current_user_id();

SELECT is(
  get_my_columns(),
  (SELECT array_agg(would_id) FROM woulds WHERE is_default),
  'default columns'
);

SELECT throws_ok(
  $$
    INSERT INTO woulds (name, added_by_id)
    SELECT 'secret fourth thing', current_user_id()
  $$,
  'P0001', 'Cannot create more columns until people use your existing ones more (you have 1, you are allowed 1)',
  'limit custom columns'
);

INSERT INTO user_woulds (user_id, would_id, with_id)
SELECT them.user_id, woulds.would_id, current_user_id()
FROM users them, woulds
WHERE them.facebook_id = 'thatUser'
AND woulds.name = 'secret third thing';

SELECT is(
  num_woulds_allowed(current_user_id()),
  2::bigint,
  'allowed another column because someone is using yours'
);

SELECT throws_ok(
  $$
    INSERT INTO woulds (name, added_by_id)
    SELECT 'secret third thing', current_user_id()
  $$,
  '23505', 'duplicate key value violates unique constraint "woulds_name_key"',
  'no duplicate names'
);

SELECT lives_ok(
  $$
    INSERT INTO woulds (name, added_by_id)
    SELECT 'secret fourth thing', current_user_id()
  $$,
  'can insert another column with a new name'
);

SELECT finish();

ROLLBACK;
