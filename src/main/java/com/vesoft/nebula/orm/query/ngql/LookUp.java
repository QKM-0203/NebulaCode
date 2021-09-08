/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.query.ngql;

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.exception.ExecuteException;
import com.vesoft.nebula.orm.exception.InitException;
import com.vesoft.nebula.orm.operator.Filter;
import com.vesoft.nebula.orm.query.cypher.Lexer;
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
public class LookUp {
    private String schema;
    private List<String> filterString;
    private Map<String, Filter> conMap;
    private List<String> yield;
    private Graph graph;

    public LookUp(Graph graph) {
        this.graph = graph;
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
     * <"player.name",Logical.OR.setRelational(Relational.EQ.setValue("qkm"),Relational.EQ.setValue("SC"))>
     * it means player.name == "qkm" or player.name == "SC".</p>
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

    /**
     * what the user wants to output,if you don't pass in yield,for vertex: print all point IDs,
     * for edge: print the start id, destination id,and rank of all edges.
     *
     * @param yield pass in eg: player.name, if you alias output,pass in eg: player.name as name,
     *              if you want to Distinct you can add DISTINCT key,
     *              eg: DISTINCT player.name as name,first string add is ok.
     * @return LookUp
     */
    public LookUp yield(String... yield) {
        this.yield = Arrays.asList(yield);
        return this;
    }

    private String connectParameters() {
        if (schema == null) {
            throw new InitException("schema can not be null");
        }
        StringBuilder result = new StringBuilder();
        result.append(Lexer.LOOKUP).append(schema);
        result.append(NGqlQuery.judgeAndJoinWhere(conMap, filterString, -1));
        if (yield != null && !yield.isEmpty()) {
            result.append(Lexer.YIELD).append(" ").append(String.join(",", yield));
        }
        return result.toString().trim();
    }

    /**
     * @return all qualified
     */
    public ResultSet all() {
        String query = connectParameters();
        ResultSet resultSet = graph.run(query);
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
        return resultSet;
    }

    /**
     * @return from result get the first
     */
    public ResultSet.Record first() {
        ResultSet all = all();
        if (!all.isEmpty()) {
            return all.rowValues(0);
        }
        return null;
    }

    /**
     * count of results.
     *
     * @return count
     */
    public long count() {
        ResultSet all = all();
        if (!all.isEmpty()) {
            return all.rowsSize();
        }
        return 0;
    }

    /**
     * is there data that meets the conditions.
     *
     * @return true or false
     */
    public boolean exist() {
        return !all().isEmpty();
    }
}