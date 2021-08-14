/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.match;

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.entity.Vertex;
import com.vesoft.nebula.orm.operator.Condition;
import com.vesoft.nebula.orm.operator.EQ;
import com.vesoft.nebula.orm.operator.Sort;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

/**
 * match Vertex
 */
public class VertexMatch {
    protected Graph graph;
    private String tagName;
    private long skip;
    private long limit;
    private HashMap<String, Sort> orderBy;
    private String groupBy;
    private List<String> filterString;
    private HashMap<String, Condition> conMap;
    private HashMap<String, Object> propMap;

    protected VertexMatch(Graph graph) {
        this.graph = graph;
    }

    /**
     * @param tagName tagname
     * @param propMap if you create tag index,you can pass in propMap
     *                eg: match (v:player{name: "qkm"})
     * @return this
     */
    public VertexMatch init(String tagName, HashMap<String, Object> propMap) {
        this.tagName = tagName;
        this.propMap = propMap;
        return this;
    }

    /**
     * filter condition
     *
     * @param conMap       String is propName,Condition is {@link Condition}
     * @param filterString filterString is alternative ,you can pass in "v.name=qkm",
     *                     for conMap you can pass in <"name",{@link EQ#EQ(Object)}>
     *                     they are common mean.
     * @return VertexMatch
     */
    public VertexMatch where(HashMap<String, Condition> conMap, String... filterString) {
        this.conMap = conMap;
        this.filterString = Arrays.asList(filterString);
        return this;
    }

    /**
     * @param skip return from line skip of the result
     * @return VertexMatch
     */
    public VertexMatch skip(long skip) {
        this.skip = skip;
        return this;
    }

    /**
     * @param orderBy sort by one or multiple attribute,pass in eg: (v.name,Sort.ASC)
     * @return VertexMatch
     */
    public VertexMatch orderBy(HashMap<String, Sort> orderBy) {
        this.orderBy = orderBy;
        return this;
    }

    /**
     * @param name pass in eg:v.name
     * @return VertexMatch
     */
    public VertexMatch groupBy(String name) {
        this.groupBy = name;
        return this;
    }

    /**
     * @param limit start from line 0 of the default value and end with line limit,
     *              it can be used in combination with skip,skip can be change default start value
     * @return VertexMatch
     */
    public VertexMatch limit(long limit) {
        this.limit = limit;
        return this;
    }

    /**
     * @return from result get the first Vertex
     */
    public Vertex first() {
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
     * @return all qualified vertexes
     */
    public List<Vertex> all() {
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
