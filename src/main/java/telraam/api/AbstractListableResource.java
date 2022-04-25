package telraam.api;


import telraam.database.daos.DAO;

import java.util.List;

public abstract class AbstractListableResource<T> extends AbstractResource<T> implements ListableResource<T> {
    protected AbstractListableResource(DAO<T> dao) {
        super(dao);
    }

    @Override
    public List<T> getListOf() {
        return dao.getAll();
    }
}
