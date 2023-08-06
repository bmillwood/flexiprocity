GRANT CONNECT ON DATABASE flexiprocity TO api;
CREATE TABLE users
  ( user_id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY
  , facebook_id text UNIQUE NOT NULL
  );
GRANT SELECT ON users TO api;

CREATE FUNCTION get_facebook_id() RETURNS text
  LANGUAGE sql SECURITY INVOKER STABLE PARALLEL RESTRICTED
AS $$SELECT current_setting('jwt.claims.facebookUserId', true)$$;

CREATE FUNCTION current_user_id() RETURNS bigint
  LANGUAGE sql SECURITY DEFINER STABLE PARALLEL RESTRICTED
  AS $$
    SELECT user_id FROM users
    WHERE facebook_id = get_facebook_id()
  $$;
REVOKE EXECUTE ON FUNCTION current_user_id() FROM public;
GRANT EXECUTE ON FUNCTION current_user_id() TO api;

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
REVOKE EXECUTE ON FUNCTION get_or_create_user_id() FROM public;
GRANT EXECUTE ON FUNCTION get_or_create_user_id() TO api;
