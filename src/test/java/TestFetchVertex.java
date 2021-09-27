/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.data.ValueWrapper;
import com.vesoft.nebula.ngqlbuilder.operator.Sort;
import com.vesoft.nebula.ngqlbuilder.query.ngql.FetchVertex;
import com.vesoft.nebula.ngqlbuilder.query.ngql.FetcherVertex;
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
public class TestFetchVertex extends TestDataBase {

    {
        graph.createTag(qkm1);
        graph.createTag(qkm2);
        graph.create(vertexOne);
        graph.create(vertexTwo);
        graph.create(vertexThird);
        graph.create(vertexFour);
    }

    @Test
    public void testFetchOneVertexOneTag() throws UnsupportedEncodingException {
        FetcherVertex fetcherVertex = new FetcherVertex(graph);
        FetchVertex fetchVertex = fetcherVertex.fetchVertex(1);
        ResultSet all = fetchVertex.on("QKM2").all();
        List<ValueWrapper> vertices = all.colValues("vertices_");
        assert vertices.size() == 1;
        assert fetchVertex.exist();
        HashMap<String, ValueWrapper> qkm2 = vertices.get(0).asNode().properties("QKM2");
        assert qkm2.get("name").asString().equals("qkm");
        assert qkm2.get("age").asLong() == 19;
        assert qkm2.get("birth").asDateTime().getLocalDateTimeStr()
            .equals("2002-02-03T06:12:12.000000");
    }

    @Test
    public void testFetchOneVertexMultipleTag() throws UnsupportedEncodingException {
        FetcherVertex fetcherVertex = new FetcherVertex(graph);
        FetchVertex fetchVertex = fetcherVertex.fetchVertex(1);
        ResultSet all = fetchVertex.on("QKM2", "QKM1").all();
        List<ValueWrapper> vertices = all.colValues("vertices_");
        assert vertices.size() == 1;
        assert fetchVertex.exist();
        HashMap<String, ValueWrapper> qkm2 = vertices.get(0).asNode().properties("QKM2");
        HashMap<String, ValueWrapper> qkm1 = vertices.get(0).asNode().properties("QKM1");
        assert qkm1.isEmpty();
        assert qkm2.get("name").asString().equals("qkm");
        assert qkm2.get("age").asLong() == 19;
        assert qkm2.get("birth").asDateTime().getLocalDateTimeStr()
            .equals("2002-02-03T06:12:12.000000");
    }

    @Test
    public void testFetchMultipleVertexMultipleTag() throws UnsupportedEncodingException {
        FetcherVertex fetcherVertex = new FetcherVertex(graph);
        ArrayList<Integer> ids = new ArrayList<>();
        ids.add(1);
        ids.add(2);
        ids.add(3);
        ids.add(5);
        FetchVertex fetchVertex = fetcherVertex.fetchVertex(ids);
        ResultSet all = fetchVertex.on("QKM2", "QKM1").all();
        List<ValueWrapper> vertices = all.colValues("vertices_");
        assert fetchVertex.exist();
        assert vertices.size() == 3;
        assert vertices.get(0).asNode().properties("QKM2").get("name").asString().equals("sc");
        assert vertices.get(1).asNode().properties("QKM2").get("name").asString().equals("qkm");
        assert vertices.get(2).asNode().properties("QKM2").get("name").asString().equals("sy");
    }

    @Test
    public void testFetchAddYield() throws UnsupportedEncodingException {
        FetcherVertex fetcherVertex = new FetcherVertex(graph);
        FetchVertex fetchVertex = fetcherVertex.fetchVertex(1);
        ResultSet all = fetchVertex.on("QKM2").yield("QKM2.name as name", "QKM2.age as age").all();
        assert all.rowsSize() == 1;
        assert fetchVertex.exist();
        List<ValueWrapper> vid = all.colValues("VertexID");
        List<ValueWrapper> name = all.colValues("name");
        List<ValueWrapper> age = all.colValues("age");
        assert vid.get(0).asLong() == 1;
        assert name.get(0).asString().equals("qkm");
        assert age.get(0).asLong() == 19;
    }

    @Test
    public void testFetchAddYieldAddOrderByAddLimit() throws UnsupportedEncodingException {
        FetcherVertex fetcherVertex = new FetcherVertex(graph);
        ArrayList<Integer> ids = new ArrayList<>();
        ids.add(1);
        ids.add(2);
        ids.add(3);
        FetchVertex fetchVertex = fetcherVertex.fetchVertex(ids);
        HashMap<String, Sort> orderBy = new HashMap<>();
        orderBy.put("age", Sort.ASC);
        ResultSet all = fetchVertex.on("QKM2").yield("QKM2.name as name", "QKM2.age as age")
            .orderBy(null,orderBy,null).limit(1,3).all();
        assert all.rowsSize() == 2;
        assert fetchVertex.exist();
        List<ValueWrapper> vid = all.colValues("VertexID");
        List<ValueWrapper> name = all.colValues("name");
        List<ValueWrapper> age = all.colValues("age");
        assert vid.get(0).asLong() == 1;
        assert name.get(0).asString().equals("qkm");
        assert age.get(0).asLong() == 19;
        assert vid.get(1).asLong() == 3;
        assert name.get(1).asString().equals("sy");
        assert age.get(1).asLong() == 20;
    }

    @Test
    public void testFetchIdException() {
        FetcherVertex fetcherVertex = new FetcherVertex(graph);
        FetchVertex fetchVertex = fetcherVertex.fetchVertex(null);
        try {
            ResultSet all = fetchVertex.on("QKM2")
                .yield("QKM2.name as name", "QKM2.age as age").all();
        } catch (Exception e) {
            assert e.getMessage().equals("vidList can not be null");
        }
    }
}
