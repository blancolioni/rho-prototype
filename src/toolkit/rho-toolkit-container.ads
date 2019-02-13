private with Css;

with Rho.Toolkit.Buildable;
with Rho.Toolkit.Widget.Lists;

package Rho.Toolkit.Container is

   type Rho_Container_Record is
     abstract new Rho.Toolkit.Widget.Rho_Widget_Record
   with private;

   type Rho_Container is access all Rho_Container_Record'Class;

   overriding procedure Initialize
     (Container : in out Rho_Container_Record);

   overriding procedure Adjust
     (Container : in out Rho_Container_Record);

   overriding function Default_Tag
     (Container : Rho_Container_Record)
      return String
   is ("Container");

   overriding function Widget_Hierarchy_Tags
     (Container : Rho_Container_Record)
      return String
   is ("Container "
       & Rho.Toolkit.Widget.Rho_Widget_Record (Container)
       .Widget_Hierarchy_Tags);

   overriding function Child_Count
     (Container : Rho_Container_Record)
      return Natural;

   overriding function Child
     (Container : Rho_Container_Record;
      Index     : Positive)
      return Rho.Toolkit.Widget.Rho_Widget;

   overriding procedure Add_Child
     (Container : not null access Rho_Container_Record;
      Child     : not null access
        Rho.Toolkit.Buildable.Rho_Buildable_Interface'Class);

   procedure Delete_Child
     (Container : not null access Rho_Container_Record;
      Child     : not null access Rho.Toolkit.Widget.Rho_Widget_Record'Class);

   procedure Delete_Children
     (Container : not null access Rho_Container_Record);

   procedure Iterate_Children
     (Container : Rho_Container_Record;
      Callback : not null access
        procedure (Widget : Rho.Toolkit.Widget.Rho_Widget));

   function Has_Children
     (Container : Rho_Container_Record)
      return Boolean;

   function First_Child
     (Container : Rho_Container_Record)
      return Rho.Toolkit.Widget.Rho_Widget
   with Pre => Container.Has_Children;

   overriding procedure Mouse_Over
     (Container : in out Rho_Container_Record;
      X, Y      : Rho_Float);

   overriding procedure Mouse_Out
     (Container : in out Rho_Container_Record);

   overriding function Get_Child_Widget
     (Container : Rho_Container_Record;
      X, Y      : Rho_Float)
      return Rho.Toolkit.Widget.Rho_Widget;

   procedure Add
     (Container : not null access Rho_Container_Record;
      Child     : not null access Rho.Toolkit.Widget.Rho_Widget_Record'Class);

   overriding procedure Show_All
     (Container : in out Rho_Container_Record);

private

   type Rho_Container_Record is
     abstract new Rho.Toolkit.Widget.Rho_Widget_Record with
      record
         Children : Rho.Toolkit.Widget.Lists.List;
      end record;

   overriding function Child_Elements
     (Container : Rho_Container_Record)
      return Css.Array_Of_Elements;

   overriding function Child_Count
     (Container : Rho_Container_Record)
      return Natural
   is (Natural (Container.Children.Length));

end Rho.Toolkit.Container;
