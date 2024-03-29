package telraam.api;


import io.swagger.v3.oas.annotations.Operation;
import telraam.database.daos.DAO;

import java.util.List;

public abstract class AbstractListableResource<T> extends AbstractResource<T> implements ListableResource<T> {
    protected AbstractListableResource(DAO<T> dao) {
        super(dao);
    }

    @Override
    @Operation(summary = "Find all")
    public List<T> getListOf() {
        return dao.getAll();
    }
}
