/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.match;

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.entity.Relationship;
import com.vesoft.nebula.orm.entity.Vertex;
import com.vesoft.nebula.orm.operator.Condition;
import com.vesoft.nebula.orm.operator.Direction;
import com.vesoft.nebula.orm.operator.EQ;
import com.vesoft.nebula.orm.operator.Sort;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

public class RelationshipMatch {
    protected Graph graph;
    private Vertex startVertex;
    private Vertex endVertex;
    private List<String> edges;
    private List<String> filterString;
    private long skip;
    private long limit;
    private HashMap<String, Sort> orderBy;
    private String groupBy;
    private Direction direction = Direction.OUT;
    private HashMap<String, Condition> conMap;
    private HashMap<String, Object> propMap;

    protected RelationshipMatch(Graph graph) {
        this.graph = graph;
    }

    /**
     * @param startVertex startVertex,can be null
     * @param endVertex   endVertex,can be null
     * @param direction   in edge or out edge
     * @param propMap     if you create edge index,you can pass in ,eg:
     *                    match (v)-[e:player{name: "qkm"}]-(v2)
     * @param types       edgeName,can be multiple
     * @return RelationshipMatch
     */
    public RelationshipMatch init(Vertex startVertex, Vertex endVertex,
                                  Direction direction, HashMap<String, Object> propMap,
                                  String... types) {
        this.startVertex = startVertex;
        this.endVertex = endVertex;
        this.propMap = propMap;
        this.edges = Arrays.asList(types);
        this.direction = direction;
        return this;
    }

    /**
     * filter condition
     *
     * @param conMap       String is propName,Condition is {@link Condition}
     * @param filterString filterString is alternative ,you can pass in
     *                     "e.name == "qkm" or v.name == "qkm",for conMap,you can
     *                     pass in <"name",{@link EQ#EQ(Object)}>,
     *                     pass in conMap means e.name == "qkm"
     * @return RelationshipMatch
     */
    public RelationshipMatch where(HashMap<String, Condition> conMap, String... filterString) {
        this.conMap = conMap;
        this.filterString = Arrays.asList(filterString);
        return this;
    }

    /**
     * @param skip return from line skip of the result
     * @return RelationshipMatch
     */
    public RelationshipMatch skip(long skip) {
        this.skip = skip;
        return this;
    }

    /**
     * @param orderBy sort by one or multiple attribute,pass in eg: (e.name,Sort.ASC)
     * @return RelationshipMatch
     */
    public RelationshipMatch orderBy(HashMap<String, Sort> orderBy) {
        this.orderBy = orderBy;
        return this;
    }

    /**
     * @param name pass in eg:e.name
     * @return RelationshipMatch
     */
    public RelationshipMatch groupBy(String name) {
        this.groupBy = name;
        return this;
    }

    /**
     * @param limit start from line 0 of the default value and end with line limit,
     *              it can be used in combination with skip,skip can be change default start value
     * @return RelationshipMatch
     */
    public RelationshipMatch limit(long limit) {
        this.limit = limit;
        return this;
    }

    /**
     * @return from result get the first relationship
     */
    public Relationship first() {
        return all().get(0);
    }

    /**
     * connect parameters
     *
     * @return sentence
     */
    private String connectQueryParameters() {
        return "";
    }

    /**
     * @return all qualified relationships
     */
    public List<Relationship> all() {
        String s = connectQueryParameters();
        ResultSet run = graph.run(s);
        return null;
    }

    /**
     * count of results
     *
     * @return count
     */
    public long count() {
        return all().size();
    }

    /**
     * is there data that meets the conditions
     *
     * @return true or false
     */
    public boolean exist() {
        return count() > 0;
    }
}
