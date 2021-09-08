/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.orm.datatype.Date;
import com.vesoft.nebula.orm.datatype.DateTime;
import com.vesoft.nebula.orm.datatype.Time;
import com.vesoft.nebula.orm.entity.*;
import com.vesoft.nebula.orm.operator.DataType;
import java.util.HashMap;
import org.junit.Test;

/**
 * because the schema creation and index creation steps are implemented asynchronously,
 * the Nepal graph cannot be created until the next heartbeat cycle,
 * so you can wait and execute the method again.
 */
public class TestCreate extends TestDataBase {

    {
        graph.createTag(qkm1);
        graph.createTag(qkm2);
        graph.createEdge(team);
        graph.createEdge(work);
    }

    @Test
    public void testCreateSpace() {
        Space testOne = new Space("test1", 10, 1, DataType.FIXED_STRING.setLength(30));
        Space testTwo = new Space("test2", 10, 1, DataType.INT64);
        graphService.createSpace(testOne);
        graphService.createSpace(testTwo);
        assert null != graphService.getGraph("test1");
        assert null != graphService.getGraph("test2");
    }

    @Test
    public void testCreateVertex() {
        graph.create(vertexTwo);
        graph.create(vertexOne);
        assert vertexTwo.getGraph() == graph;
        assert vertexOne.getGraph() == graph;
        assert graph.exists(vertexOne);
        assert graph.exists(vertexTwo);
    }

    @Test
    public void testCreateRelationship() {
        graph.create(relationship12);
        graph.create(relationship32);
        assert relationship12.getGraph() == graph;
        assert relationship32.getGraph() == graph;
        assert graph.exists(relationship12);
        assert graph.exists(relationship32);
    }

    @Test
    public void testCreateSubgraph() {
        graph.create(subgraph);
        assert subgraph.getVertexes().get(0).getGraph() == graph;
        assert subgraph.getRelationships().get(0).getGraph() == graph;
        assert graph.exists(subgraph);
    }

    @Test
    public void testCreatePath() {
        graph.create(path);
        assert path.getStartVertex().getGraph() == graph;
        assert path.getRelationships().get(0).getGraph() == graph;
        assert graph.exists(path);
    }

    @Test
    public void testCreateHasExpiredSchema() {
        Schema tag = new Schema("QKM3", tagProperties, 1000, "age");
        Schema edge = new Schema("QKM4", edgeProperties, 0, null);
        graph.createEdge(edge);
        graph.createTag(tag);
        System.out.println(graph.getEdges());
        assert graph.getEdges().contains(edge.getName());
        assert graph.getTags().contains(tag.getName());
    }

    @Test
    public void createEdgeIndex() {
        Schema edge = new Schema("QKM4", edgeProperties, 0, null);
        graph.createEdge(edge);
        HashMap<String, Integer> edgeIndexes = new HashMap<>();
        edgeIndexes.put("teacherName", 10);
        edgeIndexes.put("object", 10);
        graph.createEdgeIndex("QKM4", "i_QKM4_teacherName_object", edgeIndexes);
        assert graph.getEdgeIndexes("QKM4").contains("i_QKM4_teacherName_object");
    }

    @Test
    public void createTagIndex() {
        Schema tag = new Schema("QKM3", tagProperties, 0, null);
        graph.createTag(tag);
        graph.createTagIndex("QKM3", "i_QKM3", null);
        assert graph.getTagIndexes("QKM3").contains("i_QKM3");
    }

    @Test
    public void createTimeType() {
        DateTime dateTime = new DateTime("2021-08-06T12:23:45:123");
        Time time = new Time("12:23:45:123");
        long timestamp = 1234241312;
        Date date = new Date("2021-02-04");
    }
}
