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
import com.vesoft.nebula.orm.operator.*;
import com.vesoft.nebula.orm.query.cypher.Encoding;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

/**
 * <p>you can use {@link #where(HashMap, String...)} for conditional filtering,
 * use {@link #limit(long, long)}、{@link #groupBy(List, List)}}
 * and {@link #orderBy(HashMap)} to operate the output results,
 * call {@link #yield(List)}} as finally result.</p>
 * <p>the user does need to consider the calling order,for where and final yield
 * user can not need to consider.</p>
 */
public class Go {
    private List<Object> srcIds;
    private List<String> edges;
    private List<String> yield;
    private HashMap<String, Filter> conMap;
    private List<String> filterString;
    private long leftSteps = 0;
    private long rightSteps = 1;
    private PathDirection pathDirection;
    private StringBuilder condition = new StringBuilder();
    private final Graph graph;

    protected Go(Graph graph) {
        this.graph = graph;
    }

    protected void init(List<?> srcIds, List<String> edges) {
        this.srcIds = (List<Object>) srcIds;
        this.edges = edges;
    }

    /**
     * from m to n steps,if pass in negative means from 0 to 1.
     *
     * @param leftSteps  default 0
     * @param rightSteps default 1
     * @return Go
     */
    public Go step(long leftSteps, long rightSteps) {
        if (leftSteps >= 0) {
            this.leftSteps = leftSteps;
        }
        if (rightSteps >= 0) {
            this.rightSteps = rightSteps;
        }
        return this;
    }

    /**
     * edge direction.
     *
     * @param pathDirection default is out
     * @return Go
     */
    public Go direction(PathDirection pathDirection) {
        this.pathDirection = pathDirection;
        return this;
    }


    /**
     * filter condition,finally, conMap and filterString do logical sum operations.
     *
     * <p>For conMap,if you represents a relationship ,you can pass in
     * <"name",Relational.EQ.setValue("qkm")>it means name == "qkm".</p>
     * <p>if you represents a logic relationship you can pass in
     * <"name",Logical.OR.setRelational(Relational.EQ.setValue("qkm"),Relational.EQ.setValue("SC"))>
     * it means name == "qkm" or name == "SC";</p>
     * <p>all map elements represents an and logical relationship</p>
     *
     * @param conMap       String is propName,Relational is {@link Filter}
     *                     include (Relational、Logical、UnaryOperate)
     * @param filterString filterString is alternative ,you can pass in
     *                     "name == "qkm"".TODO check format.
     * @return Go
     */
    public Go where(HashMap<String, Filter> conMap, String... filterString) {
        this.conMap = conMap;
        this.filterString = Arrays.asList(filterString);
        return this;
    }

    /**
     * what the user wants to output.
     *
     * @param yield pass in eg:$$.team.name, if you alias output,pass in eg:$$.team.name as name.
     * @return Go
     */
    public Go yield(List<String> yield) {
        this.yield = yield;
        return this;
    }


    /**
     * <p>orderBy is used to sort,you can pass in eg: (alias,{@link Sort}),
     * Sort</p>
     *
     * @param orderBy sort by one or multiple attribute.String is alias at after yield field
     * @return Go
     */
    public Go orderBy(HashMap<String, Sort> orderBy) {
        condition.append("| ORDER BY ").append(NGqlQuery.joinOrderBy(orderBy)).append(" ");
        return this;
    }

    /**
     * achieve group by use aggregateFunctions.eg: | group by $$-.name [as name]
     * yield $$-.name [as name],max(v.age) [as maxAge].
     *
     * <p>groupBy field is used to grouping,this is put on group by.
     * pass in groupBy and aggregateFunctions are put on yield,alias is optional</>
     *
     * @param groupBy            for grouping
     * @param aggregateFunctions for calculation
     * @return RelationshipMatch
     */
    public Go groupBy(List<Column> groupBy, List<Column> aggregateFunctions) {
        condition.append(NGqlQuery.joinGroupBy(groupBy, aggregateFunctions)).append(" ");
        return this;
    }

    /**
     * get the number of rows of the result,if you pass in offsetValue is negative show all result
     * if offsetValue is positive number and numberRows is negative show from 0 to offsetValue rows.
     *
     * @param offsetValue start from line 0 of the default value and end with line limit.
     * @param numberRows  some rows
     * @return Go
     */
    public Go limit(long offsetValue, long numberRows) {
        if (offsetValue >= 0) {
            condition.append("| LIMIT ").append(offsetValue);
            if (numberRows >= 0) {
                condition.append(",").append(numberRows).append(" ");
            }
        }
        return this;
    }

    /**
     * @return from result get the first
     */
    public ResultSet.Record first() {
        if (all().rowsSize() != 0) {
            return all().rowValues(0);
        }
        return null;
    }

    /**
     * connect parameters.
     *
     * @return sentence
     */
    private String connectParameters() {
        StringBuilder result = new StringBuilder();
        result.append("GO ").append(leftSteps).append(" TO ").append(rightSteps).append(" STEPS");
        if (srcIds == null || srcIds.isEmpty()) {
            throw new InitException("srcIds can not be null");
        }
        result.append(" FROM ").append(Encoding.encodeIdList(srcIds));
        if (edges == null || edges.isEmpty()) {
            throw new InitException("edges can not be null");
        }
        result.append(" OVER ").append(String.join(",", edges)).append(" ");
        if (pathDirection != null) {
            result.append(pathDirection).append(" ");
        }
        result.append(NGqlQuery.judgeAndJoinWhere(conMap, filterString, -1));
        result.append(" YIELD ").append(String.join(",", yield));
        if (condition != null) {
            result.append(" ").append(condition);
        }
        return result.toString();
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
     * count of results.
     *
     * @return count
     */
    public long count() {
        return all().rowsSize();
    }

    /**
     * is there data that meets the conditions.
     *
     * @return true or false
     */
    public boolean exist() {
        return count() > 0;
    }
}
