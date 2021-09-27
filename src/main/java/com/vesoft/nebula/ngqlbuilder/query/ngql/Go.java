/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.ngqlbuilder.query.ngql;

import com.vesoft.nebula.ngqlbuilder.entity.Graph;
import com.vesoft.nebula.ngqlbuilder.exception.InitException;
import com.vesoft.nebula.ngqlbuilder.operator.*;
import com.vesoft.nebula.ngqlbuilder.query.cypher.Encoding;
import com.vesoft.nebula.ngqlbuilder.query.util.KeyWord;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * <p>you can use {@link #where(Map, String...)} for conditional filtering,
 * use {@link #limit(long, long)}、{@link #groupBy(List, List)}}
 * and {@link com.vesoft.nebula.ngqlbuilder.query.QueryBase#orderBy(Map, Map, Boolean)}
 * to operate the output results,call {@link NGqlQuery#yield(String...)}}
 * as finally result.</p>
 * <p>the user does need to consider the calling order,for where and final yield
 * user can not need to consider.</p>
 * <p>the simple sentence of query is similar to 'GO FROM "player102" OVER serve'.</p>
 *
 * @author Qi Kai Meng
 */
public class Go extends NGqlQuery<Go> {
    private List<Object> srcIds;
    private List<String> edges;
    private Map<String, Filter> conMap;
    private List<String> filterString;
    private long leftSteps = 0;
    private long rightSteps = 1;
    private PathDirection pathDirection;
    private StringBuffer clause = new StringBuffer();

    protected Go(Graph graph) {
        super(graph);
    }

    public StringBuffer getCondition() {
        return clause;
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
     * <"player.name",Logical.OR.setRelational(Relational.EQ.setValue("qkm"),
     * Relational.EQ.setValue("SC"))> it means player.name == "qkm" or player.name == "SC";</p>
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
     * achieve group by use aggregateFunctions.eg: | group by $$-.name [as name]
     * yield $$-.name [as name],max(v.age) [as maxAge].
     *
     * <p>groupBy field is used to grouping,this is put on group by.
     * pass in groupBy and aggregateFunctions are put on yield,alias is optional</p>
     *
     * @param groupBy            for grouping
     * @param aggregateFunctions for calculation
     * @return RelationshipMatch
     */
    public Go groupBy(List<Column> groupBy, List<Column> aggregateFunctions) {
        clause.append(joinGroupBy(groupBy, aggregateFunctions));
        return this;
    }

    /**
     * connect parameters.
     *
     * @return sentence
     */
    public String connectParameters() {
        StringBuilder result = new StringBuilder();
        result.append(KeyWord.GO).append(" ").append(leftSteps)
            .append(" ").append(KeyWord.TO).append(" ")
            .append(rightSteps).append(" ").append(KeyWord.STEPS);
        if (srcIds == null || srcIds.isEmpty()) {
            throw new InitException("srcIds can not be null");
        }
        result.append(" ").append(KeyWord.FROM).append(" ").append(Encoding.encodeIdList(srcIds));
        if (edges == null || edges.isEmpty()) {
            result.append(" ").append(KeyWord.OVER).append(" ").append(KeyWord.ALL);
        } else {
            result.append(" ").append(KeyWord.OVER).append(" ").append(String.join(",", edges));
        }
        if (pathDirection != null) {
            result.append(" ").append(pathDirection);
        }
        result.append(judgeAndJoinWhere(conMap, filterString, -1));
        if (yields != null && !yields.isEmpty()) {
            result.append(" ").append(KeyWord.YIELD).append(" ").append(String.join(",", yields));
        }
        if (clause != null) {
            result.append(clause);
        }
        return result.toString().trim();
    }

}
