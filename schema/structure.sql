GRANT CONNECT ON DATABASE flexiprocity TO api;

CREATE FUNCTION get_facebook_id() RETURNS text
  LANGUAGE sql SECURITY INVOKER STABLE PARALLEL RESTRICTED
AS $$SELECT current_setting('jwt.claims.facebookUserId', true)$$;

CREATE TYPE audience AS ENUM
  ( 'self'
  , 'everybody'
  );

CREATE TABLE users
  ( user_id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY
  , facebook_id text UNIQUE
  , name text
  , bio text NOT NULL DEFAULT ''
  , visible_to audience NOT NULL DEFAULT 'self'
  , fbid_visible_to audience
  );

CREATE FUNCTION current_user_id() RETURNS bigint
  LANGUAGE sql SECURITY DEFINER STABLE PARALLEL RESTRICTED
  AS $$
    SELECT user_id FROM users
    WHERE facebook_id = get_facebook_id()
  $$;
REVOKE EXECUTE ON FUNCTION current_user_id FROM public;
GRANT EXECUTE ON FUNCTION current_user_id TO api;

CREATE VIEW user_profiles AS
  SELECT
    user_id
  , CASE
      WHEN user_id = current_user_id() THEN facebook_id
      WHEN fbid_visible_to <> 'everybody' THEN NULL
      ELSE facebook_id
    END AS facebook_id
  , name
  , bio
  FROM users
  WHERE visible_to = 'everybody' OR user_id = current_user_id();
GRANT SELECT ON user_profiles TO api;

CREATE FUNCTION update_me(bio text, visible_to audience) RETURNS users
  LANGUAGE sql SECURITY DEFINER VOLATILE PARALLEL RESTRICTED
  AS $$
    UPDATE users
    SET bio = COALESCE(update_me.bio, users.bio)
    , visible_to = COALESCE(update_me.visible_to, users.visible_to)
    WHERE user_id = current_user_id()
    RETURNING *;
  $$;
REVOKE EXECUTE ON FUNCTION update_me FROM public;
GRANT EXECUTE ON FUNCTION update_me TO api;

CREATE FUNCTION get_or_create_user_id() RETURNS bigint
  LANGUAGE sql SECURITY DEFINER VOLATILE PARALLEL RESTRICTED
  AS $$
    WITH jwt AS (
      SELECT get_facebook_id() AS facebook_id
    )
    , created AS (
      INSERT INTO users (facebook_id)
      SELECT facebook_id FROM jwt WHERE jwt.facebook_id IS NOT NULL
      EXCEPT
      SELECT facebook_id FROM users
      RETURNING user_id
    )
    SELECT user_id FROM users
    JOIN jwt USING (facebook_id)
    UNION ALL
    SELECT user_id FROM created
  $$;
REVOKE EXECUTE ON FUNCTION get_or_create_user_id FROM public;
GRANT EXECUTE ON FUNCTION get_or_create_user_id TO api;
