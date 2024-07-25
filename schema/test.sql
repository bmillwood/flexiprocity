SET client_min_messages TO WARNING;
CREATE EXTENSION IF NOT EXISTS pgtap;
RESET client_min_messages;

BEGIN;

SET search_path = mock,pg_catalog,public;

SELECT plan(22);

SET client_min_messages TO WARNING;
TRUNCATE TABLE users, user_columns, woulds, user_woulds CASCADE;
RESET client_min_messages;

-- these don't particularly have to match the real defaults, we just want to
-- test the defaulting mechanism
INSERT INTO woulds (name, is_default)
VALUES ('Hang out sometime', true), ('Go on a date or something', true);

SET ROLE api;

SELECT is(
  get_or_create_user_id(),
  NULL,
  'get_or_create_user_id() with no credentials'
);

SELECT set_config('jwt.claims.facebookUserId', 'aliceId', true);

SELECT isnt_empty(
  $$ SELECT u FROM (SELECT get_or_create_user_id() AS u) r
     WHERE u IS NOT NULL
  $$,
  'get_or_create_user_id() works'
);

SELECT lives_ok(
  $$ SELECT update_me(name => 'Alice') $$,
  'can update name'
);

SELECT bag_eq(
  $$ SELECT name FROM my_user() me $$,
  $$ VALUES ('Alice') $$,
  'my_user returns new name'
);

RESET ROLE; -- can't select or update users directly

SELECT bag_eq(
  $$ SELECT current_user_id() $$,
  $$ SELECT user_id FROM users $$,
  'current_user_id() returns id of user'
);

INSERT INTO users (facebook_id, name, visible_to) VALUES ('bobId', 'Bob', 'everyone');

SET ROLE api;

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

SELECT lives_ok(
  $$ SELECT set_my_columns(columns => ARRAY[
      (SELECT would_id FROM woulds WHERE name = 'Hang out sometime'),
      (SELECT would_id FROM woulds WHERE name = 'secret third thing')
    ])
  $$,
  'can set columns'
);

SELECT is(
  get_my_columns(),
  ARRAY[
    (SELECT would_id FROM woulds WHERE name = 'Hang out sometime'),
    (SELECT would_id FROM woulds WHERE name = 'secret third thing')
  ],
  'columns are as set'
);

SELECT lives_ok(
  $$ SELECT set_my_columns(columns => ARRAY[
      (SELECT would_id FROM woulds WHERE name = 'secret third thing'),
      (SELECT would_id FROM woulds WHERE name = 'Hang out sometime')
    ])
  $$,
  'can set columns in a different order'
);

SELECT is(
  get_my_columns(),
  ARRAY[
    (SELECT would_id FROM woulds WHERE name = 'secret third thing'),
    (SELECT would_id FROM woulds WHERE name = 'Hang out sometime')
  ],
  'columns are as set'
);

SELECT bag_eq(
  $$ SELECT name, uses FROM would_stats $$,
  $$ VALUES
    ('Hang out sometime', NULL),
    ('Go on a date or something', NULL),
    ('secret third thing', 1)
  $$,
  'count'
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

RESET ROLE;
SELECT is_empty(
  $$ SELECT * FROM email_sending $$,
  'no e-mails yet'
);
SET ROLE api;

SELECT isnt_empty(
  $$
    INSERT INTO user_woulds (user_id, would_id, with_id)
    SELECT current_user_id(), w.would_id, them.user_id
    FROM woulds w, user_profiles them
    WHERE w.name = 'Hang out sometime'
    AND them.facebook_id = 'bobId'
    RETURNING *
  $$,
  'can express interest'
  );

SELECT throws_ok(
  $$
    INSERT INTO user_woulds (user_id, would_id, with_id)
    SELECT them.user_id, w.would_id, current_user_id()
    FROM woulds w, user_profiles them
    WHERE w.name = 'Hang out sometime'
    AND them.facebook_id = 'bobId'
    RETURNING *
  $$,
  '42501', 'new row violates row-level security policy for table "user_woulds"',
  'cannot ascribe interest to others'
);

RESET ROLE;
UPDATE user_woulds SET user_id = with_id, with_id = user_id;
SET ROLE api;

SAVEPOINT before_match;
SELECT isnt_empty(
  $$
    INSERT INTO user_woulds (user_id, would_id, with_id)
    SELECT current_user_id(), w.would_id, them.user_id
    FROM woulds w, user_profiles them
    WHERE w.name = 'Hang out sometime'
    AND them.facebook_id = 'bobId'
    RETURNING *
  $$,
  'can express reciprocated interest'
);

RESET ROLE;
SELECT is_empty(
  $$ SELECT * FROM email_sending $$,
  'still no e-mails yet'
);
ROLLBACK TO before_match;
RESET ROLE;
WITH user_emails AS (
  SELECT user_id, lower(name) || '@host.example' AS email_address
  FROM users
), new_contacts AS (
  INSERT INTO contacts (email_address)
  SELECT email_address
  FROM user_emails
  RETURNING contact_id, email_address
)
UPDATE users
SET send_email_on_matches = TRUE
  , verified_contact_id = contact_id
FROM user_emails ue, new_contacts nc
WHERE users.user_id = ue.user_id
AND ue.email_address = nc.email_address;
SET ROLE api;

SELECT isnt_empty(
  $$
    INSERT INTO user_woulds (user_id, would_id, with_id)
    SELECT current_user_id(), w.would_id, them.user_id
    FROM woulds w, user_profiles them
    WHERE w.name = 'Hang out sometime'
    AND them.facebook_id = 'bobId'
    RETURNING *
  $$,
  'can express reciprocated interest'
);

RESET ROLE;
SELECT bag_eq(
  $$
    SELECT c1.email_address AS e1, c2.email_address AS e2, ma.name1, ma.name2, ma.would_matches
    FROM email_sending
    LEFT JOIN matches ma USING (match_id)
    LEFT JOIN contacts c1 ON (c1.contact_id = ma.contact_id1)
    LEFT JOIN contacts c2 ON (c2.contact_id = ma.contact_id2)
  $$,
  $$ VALUES ('alice@host.example', 'bob@host.example', 'Alice', 'Bob', ARRAY['Hang out sometime']) $$,
  'email queued'
);
SET ROLE api;

SELECT finish();

ROLLBACK;
