/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.query.ngql;

import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.exception.InitException;
import com.vesoft.nebula.orm.operator.Filter;
import com.vesoft.nebula.orm.query.util.KeyWord;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * <p>list points by label: retrieves all point IDs for the specified label.</p>
 * <p>list edges by edge type: retrieves the start, destination,
 * and rank of all edges of the specified edge type.</p>
 * <p>note: make sure that at least one index is available for the lookup statement.</p>
 * <p>the sentence of query is similar to 'LOOKUP ON player WHERE player.name == "Tony Parker"'.</p>
 *
 * @author Qi Kai Meng
 */
public class LookUp extends NGqlQuery<LookUp> {
    private String schema;
    private List<String> filterString;
    private Map<String, Filter> conMap;
    private StringBuffer clause = new StringBuffer();

    protected LookUp(Graph graph) {
        super(graph);
    }

    public StringBuffer getClause() {
        return clause;
    }


    protected LookUp init(String schema) {
        this.schema = schema;
        return this;
    }

    /**
     * filter condition,finally, conMap and filterString do logical sum operations.
     *
     * <p>for conMap,you can pass in
     * <"player.name",Relational.EQ.setValue("qkm")>it means player.name == "qkm".</p>
     * <p>if you represents a logic relationship you can pass in
     * <"player.name",Logical.OR.setRelational(Relational.EQ.setValue("qkm"),
     * Relational.EQ.setValue("SC"))> it means player.name == "qkm" or player.name == "SC".</p>
     * <p>all map elements represents an and logical relationship.</p>
     *
     * @param conMap       String is propName,Relational is {@link Filter}
     *                     include (Relational、Logical、UnaryOperate)
     * @param filterString filterString is alternative ,you can pass in
     *                     "player.name == "qkm"" TODO check format
     * @return LookUp
     */
    public LookUp where(Map<String, Filter> conMap, String... filterString) {
        this.conMap = conMap;
        this.filterString = Arrays.asList(filterString);
        return this;
    }

    public String connectParameters() {
        if (schema == null) {
            throw new InitException("schema can not be null");
        }
        StringBuilder result = new StringBuilder();
        result.append(KeyWord.LOOKUP).append(" ").append(schema);
        result.append(judgeAndJoinWhere(conMap, filterString, -1));
        if (yields != null && !yields.isEmpty()) {
            result.append(" ").append(KeyWord.YIELD).append(" ").append(String.join(",", yields));
        }
        if (clause != null) {
            result.append(clause);
        }
        System.out.println(result);
        return result.toString().trim();
    }
}
