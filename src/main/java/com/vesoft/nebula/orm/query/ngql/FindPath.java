/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.query.ngql;

import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.exception.InitException;
import com.vesoft.nebula.orm.operator.PathDirection;
import com.vesoft.nebula.orm.operator.PathType;
import com.vesoft.nebula.orm.query.cypher.Encoding;
import com.vesoft.nebula.orm.query.util.KeyWord;
import java.util.List;

/**
 * according to the list of start and destination points,
 * path type, edgeType and some result processing , such as order by、limit to get path.
 *
 * <p>query sentence is similar to 'FIND SHORTEST PATH FROM "player102" TO "team204" OVER *'.</p>
 * <p>the format returned is similar to (id)-[:edgeName@rank{}]->(id).</p>
 *
 * @author Qi Kai Meng
 */
public class FindPath extends NGqlQuery<FindPath> {
    private int steps = 5;
    private PathType pathType;
    private List<Object> srcIds;
    private List<Object> dstIds;
    private List<String> edges;
    private PathDirection pathDirection;
    private StringBuffer clause = new StringBuffer();

    protected FindPath(Graph graph) {
        super(graph);
    }

    public StringBuffer getClause() {
        return clause;
    }

    /**
     * @param pathType SHORTEST or NOLOOP or ALL
     * @param srcIds   srcIds start idList
     * @param dstIds   end idList
     */
    protected FindPath init(PathType pathType, List<?> srcIds, List<?> dstIds, List<String> edges) {
        this.pathType = pathType;
        this.srcIds = (List<Object>) srcIds;
        this.dstIds = (List<Object>) dstIds;
        this.edges = edges;
        return this;
    }

    /**
     * pass in pathDirection.
     *
     * @param pathDirection from->to or to->from or both
     * @return FindPath
     */
    public FindPath pathDirection(PathDirection pathDirection) {
        this.pathDirection = pathDirection;
        return this;
    }

    /**
     * pass in steps,if you pass in negative,use default value
     *
     * @param steps relationship number,default value is five
     * @return FindPath
     */
    public FindPath steps(int steps) {
        this.steps = steps;
        return this;
    }

    public String connectParameters() {
        if (srcIds == null || srcIds.isEmpty()) {
            throw new InitException("srcIds can not be null");
        }
        if (dstIds == null || dstIds.isEmpty()) {
            throw new InitException("dstIds can not be null");
        }
        if (pathType == null) {
            throw new InitException("pathType can not be null");
        }
        StringBuilder result = new StringBuilder();
        result.append(KeyWord.FIND).append(" ").append(pathType).append(KeyWord.PATH).append(" ");
        result.append(String.format(KeyWord.FROM + " %s", Encoding.encodeIdList(srcIds)));
        result.append(String.format(" " + KeyWord.TO + " " + "%s ", Encoding.encodeIdList(dstIds)));
        if (edges == null || edges.isEmpty()) {
            result.append(KeyWord.OVER).append(" ").append(KeyWord.ALL).append(" ");
        } else {
            result.append(String.format(KeyWord.OVER + " %s ", String.join(",", edges)));
        }
        if (pathDirection != null) {
            result.append(pathDirection).append(" ");
        }
        if (steps >= 0) {
            result.append(KeyWord.UPTO).append(" ").append(steps).append(" ").append(KeyWord.STEPS);
        }
        if (clause != null) {
            result.append(clause);
        }
        System.out.println(result);
        return result.toString().trim();
    }
}
