/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.ngql;

import com.vesoft.nebula.orm.entity.Relationship;
import com.vesoft.nebula.orm.util.QueryBase;
import java.util.ArrayList;
import java.util.List;

public class NGqlQuery extends QueryBase {
    public static String joinRelationshipNoHasValue(List<Relationship> relationships) {
        ArrayList<String> result = new ArrayList<>();
        for (Relationship relationship : relationships) {
            result.add(String.format("%s->%s@%s", relationship.getStartVid() instanceof String
                    ? "\"" + relationship.getStartVid() + "\"" : relationship.getStartVid(),
                relationship.getEndVid() instanceof String
                    ? "\"" + relationship.getEndVid() + "\"" : relationship.getEndVid(),
                relationship.getRank()));
        }
        return String.join(",", result);
    }

}
