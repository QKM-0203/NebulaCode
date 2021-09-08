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
import com.vesoft.nebula.orm.query.cypher.Lexer;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * <p>you can use {@link #where(Map, String...)} for conditional filtering,
 * use {@link #limit(long, long)}、{@link #groupBy(List, List)}}
 * and {@link #orderBy(Map)} to operate the output results,
 * call {@link #yield(String...)}} as finally result.</p>
 * <p>the user does need to consider the calling order,for where and final yield
 * user can not need to consider.</p>
 * <p>the simple sentence of query is similar to 'GO FROM "player102" OVER serve'.</p>
 *
 * @author Qi Kai Meng
 */
public class Go {
    private List<Object> srcIds;
    private List<String> edges;
    private List<String> yield;
    private Map<String, Filter> conMap;
    private List<String> filterString;
    private long leftSteps = 0;
    private long rightSteps = 1;
    private PathDirection pathDirection;
    private StringBuilder condition = new StringBuilder();
    private final Graph graph;

    protected Go(Graph graph) {
        this.graph = graph;
    }

    protected Go init(List<?> srcIds, List<String> edges) {
        this.srcIds = (List<Object>) srcIds;
        this.edges = edges;
        return this;
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
     * <p>For conMap,you can pass in <"player.name",Relational.EQ.setValue("qkm")>
     * it means player.name == "qkm".</p>
     * <p>if you represents a logic relationship you can pass in
     * <"player.name",Logical.OR.setRelational(Relational.EQ.setValue("qkm"),Relational.EQ.setValue("SC"))>
     * it means player.name == "qkm" or player.name == "SC";</p>
     * <p>all map elements represents an and logical relationship</p>
     *
     * @param conMap       String is propName,Relational is {@link Filter}
     *                     include (Relational、Logical、UnaryOperate)
     * @param filterString filterString is alternative ,you can pass in
     *                     "player.name == "qkm"".TODO check format.
     * @return Go
     */
    public Go where(Map<String, Filter> conMap, String... filterString) {
        this.conMap = conMap;
        this.filterString = Arrays.asList(filterString);
        return this;
    }

    /**
     * what the user wants to output,if yield is not set,the format returned is destination ID.
     *
     * @param yield pass in eg:$$.team.name, if you alias output,pass in eg:$$.team.name as name,
     *              if you want to Distinct you can add DISTINCT key,
     *              eg: DISTINCT $$.team.name as name,first string add is ok.
     * @return Go
     */
    public Go yield(String... yield) {
        this.yield = Arrays.asList(yield);
        return this;
    }


    /**
     * <p>orderBy is used to sort,you can pass in eg: (alias,{@link Sort}),
     * Sort</p>
     *
     * @param orderBy sort by one or multiple attribute.String is alias at after yield field
     * @return Go
     */
    public Go orderBy(Map<String, Sort> orderBy) {
        condition.append(Lexer.PIPE).append(Lexer.ORDER_BY).append(NGqlQuery.joinOrderBy(orderBy));
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
        condition.append(NGqlQuery.joinGroupBy(groupBy, aggregateFunctions));
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
            condition.append(Lexer.PIPE).append(Lexer.LIMIT).append(offsetValue);
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
        ResultSet all = all();
        if (!all.isEmpty()) {
            return all.rowValues(0);
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
        result.append(Lexer.GO).append(leftSteps).append(Lexer.TO).append(rightSteps).append(Lexer.STEPS);
        if (srcIds == null || srcIds.isEmpty()) {
            throw new InitException("srcIds can not be null");
        }
        result.append(" ").append(Lexer.FROM).append(Encoding.encodeIdList(srcIds));
        if (edges == null || edges.isEmpty()) {
            result.append(" ").append(Lexer.OVER).append(Lexer.ALL);
        } else {
            result.append(" ").append(Lexer.OVER).append(String.join(",", edges));
        }
        if (pathDirection != null) {
            result.append(" ").append(pathDirection);
        }
        result.append(NGqlQuery.judgeAndJoinWhere(conMap, filterString, -1));
        if (yield != null && !yield.isEmpty()) {
            result.append(Lexer.YIELD).append(" ").append(String.join(",", yield));
        }
        if (condition != null) {
            result.append(condition);
        }
        return result.toString().trim();
    }

    /**
     * @return all qualified
     */
    public ResultSet all() {
        String query = connectParameters();
        ResultSet resultSet = graph.run(query);
        System.out.println(query.trim());
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
