/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.orm.entity.*;
import com.vesoft.nebula.orm.operator.DataType;
import java.util.HashMap;
import org.junit.Test;

public class TestCreate extends TestDataBase {
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
        graph.create(relationship23);
        assert relationship12.getGraph() == graph;
        assert relationship23.getGraph() == graph;
        assert graph.exists(relationship12);
        assert graph.exists(relationship23);
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
    public void testCreateSchema() {
        graph.createEdge(qkm5);
        graph.createTag(qkm6);
        assert graph.getEdges().contains("\"" + qkm5.getName() + "\"");
        assert graph.getTags().contains("\"" + qkm6.getName() + "\"");
    }

    @Test
    public void createEdgeIndex() {
        graph.createEdge(qkm5);
        HashMap<String, Integer> edgeIndexes = new HashMap<>();
        edgeIndexes.put("money", null);
        edgeIndexes.put("number", null);
        graph.createEdgeIndex("QKM5", "t_qkm5", edgeIndexes);
        assert graph.getEdgeIndexes("QKM5").contains("\"t_qkm5\"");
    }

    @Test
    public void createTagIndex() {
        graph.createTag(qkm6);
        HashMap<String, Integer> tagIndexes = new HashMap<>();
        graph.createTagIndex("QKM6", "t_qkm6", null);
        assert graph.getTagIndexes("QKM6").contains("\"t_qkm6\"");
    }
}
