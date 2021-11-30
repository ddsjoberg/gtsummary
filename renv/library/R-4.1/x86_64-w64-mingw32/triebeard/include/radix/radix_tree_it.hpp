#ifndef RADIX_TREE_IT
#define RADIX_TREE_IT

#include <iterator>

// forward declaration
template <typename K, typename T> class radix_tree;
template <typename K, typename T> class radix_tree_node;

template <typename K, typename T>
class radix_tree_it : public std::iterator<std::forward_iterator_tag, std::pair<K, T> > {
    friend class radix_tree<K, T>;

public:
    radix_tree_it() : m_pointee(0) { }
    radix_tree_it(const radix_tree_it& r) : m_pointee(r.m_pointee) { }
    radix_tree_it& operator=(const radix_tree_it& r) { m_pointee = r.m_pointee; return *this; }
    ~radix_tree_it() { }

    std::pair<const K, T>& operator*  () const;
    std::pair<const K, T>* operator-> () const;
    const radix_tree_it<K, T>& operator++ ();
    radix_tree_it<K, T> operator++ (int);
    // const radix_tree_it<K, T>& operator-- ();
    bool operator!= (const radix_tree_it<K, T> &lhs) const;
    bool operator== (const radix_tree_it<K, T> &lhs) const;

private:
    radix_tree_node<K, T> *m_pointee;
    radix_tree_it(radix_tree_node<K, T> *p) : m_pointee(p) { }

    radix_tree_node<K, T>* increment(radix_tree_node<K, T>* node) const;
    radix_tree_node<K, T>* descend(radix_tree_node<K, T>* node) const;
};

template <typename K, typename T>
radix_tree_node<K, T>* radix_tree_it<K, T>::increment(radix_tree_node<K, T>* node) const
{
    radix_tree_node<K, T>* parent = node->m_parent;

    if (parent == NULL)
        return NULL;

    typename radix_tree_node<K, T>::it_child it = parent->m_children.find(node->m_key);
    assert(it != parent->m_children.end());
    ++it;

    if (it == parent->m_children.end())
        return increment(parent);
    else
        return descend(it->second);
}

template <typename K, typename T>
radix_tree_node<K, T>* radix_tree_it<K, T>::descend(radix_tree_node<K, T>* node) const
{
    if (node->m_is_leaf)
        return node;

    typename radix_tree_node<K, T>::it_child it = node->m_children.begin();

    assert(it != node->m_children.end());

    return descend(it->second);
}

template <typename K, typename T>
std::pair<const K, T>& radix_tree_it<K, T>::operator* () const
{
    return *m_pointee->m_value;
}

template <typename K, typename T>
std::pair<const K, T>* radix_tree_it<K, T>::operator-> () const
{
    return m_pointee->m_value;
}

template <typename K, typename T>
bool radix_tree_it<K, T>::operator!= (const radix_tree_it<K, T> &lhs) const
{
    return m_pointee != lhs.m_pointee;
}

template <typename K, typename T>
bool radix_tree_it<K, T>::operator== (const radix_tree_it<K, T> &lhs) const
{
    return m_pointee == lhs.m_pointee;
}

template <typename K, typename T>
const radix_tree_it<K, T>& radix_tree_it<K, T>::operator++ ()
{
    if (m_pointee != NULL) // it is undefined behaviour to dereference iterator that is out of bounds...
        m_pointee = increment(m_pointee);
    return *this;
}

template <typename K, typename T>
radix_tree_it<K, T> radix_tree_it<K, T>::operator++ (int)
{
    radix_tree_it<K, T> copy(*this);
    ++(*this);
    return copy;
}

#endif // RADIX_TREE_IT
