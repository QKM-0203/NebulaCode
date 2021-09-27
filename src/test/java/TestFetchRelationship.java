/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.data.ValueWrapper;
import com.vesoft.nebula.ngqlbuilder.entity.Relationship;
import com.vesoft.nebula.ngqlbuilder.operator.Sort;
import com.vesoft.nebula.ngqlbuilder.query.ngql.FetchEdge;
import com.vesoft.nebula.ngqlbuilder.query.ngql.FetcherEdge;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import org.junit.Test;

/**
 * because the schema creation and index creation steps are implemented asynchronously,
 * the Nepal graph cannot be created until the next heartbeat cycle,
 * so you can wait and execute the method again.
 */
public class TestFetchRelationship extends TestDataBase {
    {
        graph.createEdge(team);
        graph.createEdge(work);
        graph.create(relationship12);
        graph.create(relationship32);
        graph.create(relationship24);
        graph.create(relationship34);
        graph.create(relationship23);
        graph.create(relationship13);
    }

    @Test
    public void testFetchOneEdge() throws UnsupportedEncodingException {
        FetcherEdge fetcherEdge = new FetcherEdge(graph);
        FetchEdge fetchEdge = fetcherEdge.fetchEdge(relationship12);
        ResultSet one = fetchEdge.one();
        assert one.rowsSize() == 1;
        assert fetchEdge.exist();
        List<ValueWrapper> edges = one.colValues("edges_");
        HashMap<String, ValueWrapper> properties = edges.get(0).asRelationship().properties();
        assert properties.get("teacherName").asString().equals("qkm");
        assert properties.get("object").asString().equals("math");
    }

    @Test
    public void testFetchMultipleEdge1() throws UnsupportedEncodingException {
        FetcherEdge fetcherEdge = new FetcherEdge(graph);
        ArrayList<Relationship> relationships = new ArrayList<>();
        Relationship relationship34 = new Relationship(3, 4, "team", 1);
        relationships.add(relationship12);
        relationships.add(relationship24);
        relationships.add(relationship34);
        FetchEdge fetchEdge = fetcherEdge.fetchEdge(relationships);
        List<ResultSet> all = fetchEdge.fetchAll();
        assert all.size() == 1;
        assert fetchEdge.exist();
        assert fetchEdge.count() == 2;
        List<ValueWrapper> edges = all.get(0).colValues("edges_");
        HashMap<String, ValueWrapper> properties = edges.get(0).asRelationship().properties();
        assert properties.get("teacherName").asString().equals("sc");
        assert properties.get("object").asString().equals("chinese");
        HashMap<String, ValueWrapper> properties1 = edges.get(1).asRelationship().properties();
        assert properties1.get("teacherName").asString().equals("qkm");
        assert properties1.get("object").asString().equals("math");
    }

    @Test
    public void testFetchMultipleEdge2() {
        FetcherEdge fetcherEdge = new FetcherEdge(graph);
        ArrayList<Relationship> relationships = new ArrayList<>();
        Relationship relationship12 = new Relationship(1, 2, "work", 1);
        Relationship relationship34 = new Relationship(3, 4, "team", 1);
        relationships.add(relationship12);
        relationships.add(relationship24);
        relationships.add(relationship34);
        FetchEdge fetchEdge = fetcherEdge.fetchEdge(relationships);
        List<ResultSet> all = fetchEdge.fetchAll();
        assert all.size() == 2;
        assert fetchEdge.exist();
        assert fetchEdge.count() == 1;

    }

    @Test
    public void testFetchEdgeAddYield() throws UnsupportedEncodingException {
        FetcherEdge fetcherEdge = new FetcherEdge(graph);
        FetchEdge fetchEdge = fetcherEdge.fetchEdge(relationship12);
        ResultSet one = fetchEdge.yield("team.teacherName as name").one();
        assert one.rowsSize() == 1;
        assert fetchEdge.exist();
        assert fetchEdge.count() == 1;
        List<ValueWrapper> srcId = one.colValues("team._src");
        assert srcId.get(0).asLong() == 1;
        List<ValueWrapper> dstId = one.colValues("team._dst");
        assert dstId.get(0).asLong() == 2;
        List<ValueWrapper> rank = one.colValues("team._rank");
        assert rank.get(0).asLong() == 1;
        List<ValueWrapper> name = one.colValues("name");
        assert name.get(0).asString().equals("qkm");
    }

    @Test
    public void testFetchEdgeAddYieldAddOrderByAddLimit() throws UnsupportedEncodingException {
        FetcherEdge fetcherEdge = new FetcherEdge(graph);
        ArrayList<Relationship> relationships = new ArrayList<>();
        relationships.add(relationship12);
        relationships.add(relationship24);
        FetchEdge fetchEdge = fetcherEdge.fetchEdge(relationships);
        HashMap<String, Sort> orderBy = new HashMap<>();
        orderBy.put("name", Sort.DESC);
        ResultSet one = fetchEdge.yield("team.teacherName as name")
            .limit(0, 2).orderBy(null, orderBy,null).one();
        assert one.rowsSize() == 2;
        assert fetchEdge.exist();
        assert fetchEdge.count() == 2;
        List<ValueWrapper> srcId1 = one.colValues("team._src");
        assert srcId1.get(0).asLong() == 2;
        List<ValueWrapper> dstId1 = one.colValues("team._dst");
        assert dstId1.get(0).asLong() == 4;
        List<ValueWrapper> rank1 = one.colValues("team._rank");
        assert rank1.get(0).asLong() == 1;
        List<ValueWrapper> name1 = one.colValues("name");
        assert name1.get(0).asString().equals("sc");
        List<ValueWrapper> srcId2 = one.colValues("team._src");
        assert srcId2.get(1).asLong() == 1;
        List<ValueWrapper> dstId2 = one.colValues("team._dst");
        assert dstId2.get(1).asLong() == 2;
        List<ValueWrapper> rank = one.colValues("team._rank");
        assert rank.get(1).asLong() == 1;
        List<ValueWrapper> name2 = one.colValues("name");
        assert name2.get(1).asString().equals("qkm");
    }

    @Test
    public void testFetchEdgeNameIsNullException() {
        FetcherEdge fetcherEdge = new FetcherEdge(graph);
        Relationship relationship12 = new Relationship(1, 2, null, 1);
        FetchEdge fetchEdge = fetcherEdge.fetchEdge(relationship12);
        try {
            ResultSet one = fetchEdge.yield("team.teacherName as name").one();
        } catch (Exception e) {
            assert e.getMessage().equals("edgeName of (1)-[:null@1{}]->(2) is null");
        }
    }
}
