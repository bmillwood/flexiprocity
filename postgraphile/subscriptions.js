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
        type FriendUpdate {
            unit: Unit
        }
        type UserUpdate {
            profile: UserProfile
        }
        union Update = FriendUpdate | UserUpdate
        extend type Subscription {
            update: Update @pgSubscription(topic: ${embed(userTopic)})
        }
    `,
    resolvers: {
        Update: {
            __resolveType(payload) {
                switch(payload.topic) {
                case 'user':
                    return 'UserUpdate';
                case 'friend':
                    return 'FriendUpdate';
                default:
                    return null;
                }
            }
        },
        FriendUpdate: {
            unit: async (_payload, _args, _context, _resolveInfo) => {
                return null;
            }
        },
        UserUpdate: {
            profile: async (payload, args, context, resolveInfo) => {
                const rows = await resolveInfo.graphile.selectGraphQLResultFromTable(
                    pgSql.fragment`user_profiles`,
                    (tableAlias, sqlBuilder) => {
                        sqlBuilder.where(
                            pgSql.fragment`${tableAlias}.user_id = current_user_id()`
                        );
                    }
                );
                return rows[0];
            },
        },
    },
}));
