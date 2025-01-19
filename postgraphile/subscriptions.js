const { makeExtendSchemaPlugin, gql, embed } = require("graphile-utils");

const userTopic = async (_args, context, _resolveInfo) => {
  const { rows } = await context.pgClient.query(
    "SELECT user_id FROM current_user_id() u(user_id) WHERE user_id IS NOT NULL",
    []
  );
  if(rows.length == 1) {
    return `user:${rows[0].user_id}`;
  }
  return "user:anonymous";
}

module.exports = makeExtendSchemaPlugin(({ pgSql }) => ({
  typeDefs: gql`
    extend type Subscription {
      userUpdate: User @pgSubscription(topic: ${embed(userTopic)})
    }
  `,
  resolvers: {
    User: {
      message: async (payload, args, context, resolveInfo) => {
        const rows = await resolveInfo.graphile.selectGraphQLResultFromTable(
          pgSql.fragment`users`,
          (tableAlias, sqlBuilder) => {
            sqlBuilder.where(
              pgSql.fragment`${tableAlias}.user_id = current_user_id()`
            );
          }
        );
        return rows[0];
      }
    },
  },
}));
