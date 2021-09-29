/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.ngqlbuilder.entity.Schema;
import com.vesoft.nebula.ngqlbuilder.entity.Space;
import com.vesoft.nebula.ngqlbuilder.operator.DataType;
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
public class TestDelete extends TestDataBase {

    {
        graph.createTag(hobby);
        graph.createTag(person);
        graph.createEdge(subject);
        graph.createEdge(work);
    }

    @Test
    public void testDropSpace() throws UnsupportedEncodingException {
        Space testOne = new Space("test1", 10, 1, DataType.FIXED_STRING.setLength(30));
        Space testTwo = new Space("test2", 10, 1, DataType.INT64);
        graphService.createSpace(testOne);
        graphService.createSpace(testTwo);
        assert null != graphService.getGraph("test1");
        assert null != graphService.getGraph("test2");
        ArrayList<String> spaceNameList = new ArrayList<>();
        spaceNameList.add("test1");
        graphService.dropSpaces(spaceNameList);
        List<String> spaces = graphService.showSpaces();
        assert !spaces.contains("test1")
            && spaces.contains("test2");
    }

    @Test
    public void testDropVertex() {
        graph.create(vertexTwo);
        assert graph.exists(vertexTwo);
        graph.delete(vertexTwo);
        assert !graph.exists(vertexTwo);
    }

    @Test
    public void testDropRelationship() {
        graph.create(relationship12);
        assert graph.exists(relationship12);
        graph.delete(relationship12);
        assert !graph.exists(relationship12);
    }

    @Test
    public void testDropSubgraph() {
        graph.create(subgraph);
        assert graph.exists(subgraph);
        graph.delete(subgraph);
        assert !graph.exists(subgraph);
    }

    @Test
    public void testDropPath() {
        graph.create(path);
        assert graph.exists(path);
        graph.delete(path);
        assert !graph.exists(path);
    }

    @Test
    public void deleteTag() {
        Schema tag = new Schema("deletePerson", personSchema, 0, null);
        graph.createTag(tag);
        assert graph.getTags().contains(tag.getName());
        ArrayList<String> tagList = new ArrayList<>();
        tagList.add("deletePerson");
        graph.dropTagList(tagList);
        graph.dropTag("deletePerson");
        assert !graph.getTags().contains(tag.getName());
    }

    @Test
    public void deleteEdge() {
        Schema edge = new Schema("deleteTeam", subjectSchema, 0, null);
        graph.createEdge(edge);
        assert graph.getEdges().contains(edge.getName());
        ArrayList<String> edgeList = new ArrayList<>();
        edgeList.add("deleteTeam");
        graph.dropEdgeList(edgeList);
        graph.dropEdge("deleteTeam");
        assert !graph.getEdges().contains(edge.getName());
    }

    @Test
    public void deleteEdgeIndex() throws UnsupportedEncodingException {
        Schema edge = new Schema("deleteTeamIndex", subjectSchema, 0, null);
        graph.createEdge(edge);
        HashMap<String, Integer> edgeIndexes = new HashMap<>();
        edgeIndexes.put("teacherName", 10);
        graph.createEdgeIndex("deleteTeamIndex", "i_deleteTeamIndex_teacherName", edgeIndexes);
        edgeIndexes.put("object", 10);
        graph.createEdgeIndex("deleteTeamIndex", "i_deleteTeamIndex_teacherName_object", edgeIndexes);
        assert graph.getEdgeIndexes("deleteTeamIndex").contains("i_deleteTeamIndex_teacherName_object")
            && graph.getEdgeIndexes("deleteTeamIndex").contains("i_deleteTeamIndex_teacherName");
        graph.dropEdgeIndex("i_deleteTeamIndex_teacherName_object");
        assert !graph.getEdgeIndexes("deleteTeamIndex").contains("i_deleteTeamIndex_teacherName_object")
            && graph.getEdgeIndexes("deleteTeamIndex").contains("i_deleteTeamIndex_teacherName");
    }

    @Test
    public void deleteTagIndex() throws UnsupportedEncodingException {
        Schema tag = new Schema("deletePersonIndex", personSchema, 0, null);
        graph.createTag(tag);
        graph.createTagIndex("deletePersonIndex", "i_deletePersonIndex", null);
        assert graph.getTagIndexes("deletePersonIndex").contains("i_deletePersonIndex");
        graph.dropTagIndex("i_deletePersonIndex");
        assert graph.getTagIndexes("deletePersonIndex") == null;
    }

}
